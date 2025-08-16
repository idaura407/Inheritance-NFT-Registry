(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-claimed (err u102))
(define-constant err-not-time-yet (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-beneficiary (err u105))
(define-constant err-already-exists (err u106))
(define-constant err-inactive-inheritance (err u107))

(define-data-var inheritance-counter uint u0)

(define-map inheritances
    uint
    {
        owner: principal,
        beneficiary: principal,
        nft-contract: principal,
        token-id: uint,
        unlock-height: uint,
        claimed: bool,
        active: bool,
        created-at: uint,
    }
)

(define-map nft-inheritance-map
    {
        contract: principal,
        token-id: uint,
    }
    uint
)

(define-read-only (get-inheritance (inheritance-id uint))
    (map-get? inheritances inheritance-id)
)

(define-read-only (get-inheritance-by-nft
        (nft-contract principal)
        (token-id uint)
    )
    (match (map-get? nft-inheritance-map {
        contract: nft-contract,
        token-id: token-id,
    })
        inheritance-id (get-inheritance inheritance-id)
        none
    )
)

(define-read-only (get-current-block-height)
    stacks-block-height
)

(define-read-only (is-inheritance-unlocked (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (and
            (get active inheritance)
            (>= stacks-block-height (get unlock-height inheritance))
            (not (get claimed inheritance))
        )
        false
    )
)

(define-read-only (get-inheritance-status (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (if (get claimed inheritance)
            "claimed"
            (if (not (get active inheritance))
                "inactive"
                (if (>= stacks-block-height (get unlock-height inheritance))
                    "unlocked"
                    "locked"
                )
            )
        )
        "not-found"
    )
)

(define-read-only (get-total-inheritances)
    (var-get inheritance-counter)
)

(define-public (create-inheritance
        (nft-contract principal)
        (token-id uint)
        (beneficiary principal)
        (unlock-delay uint)
    )
    (let (
            (inheritance-id (+ (var-get inheritance-counter) u1))
            (unlock-height (+ stacks-block-height unlock-delay))
        )
        (asserts! (not (is-eq tx-sender beneficiary)) err-invalid-beneficiary)
        (asserts!
            (is-none (map-get? nft-inheritance-map {
                contract: nft-contract,
                token-id: token-id,
            }))
            err-already-exists
        )

        (map-set inheritances inheritance-id {
            owner: tx-sender,
            beneficiary: beneficiary,
            nft-contract: nft-contract,
            token-id: token-id,
            unlock-height: unlock-height,
            claimed: false,
            active: true,
            created-at: stacks-block-height,
        })

        (map-set nft-inheritance-map {
            contract: nft-contract,
            token-id: token-id,
        }
            inheritance-id
        )
        (var-set inheritance-counter inheritance-id)

        (ok inheritance-id)
    )
)

(define-public (update-beneficiary
        (inheritance-id uint)
        (new-beneficiary principal)
    )
    (match (get-inheritance inheritance-id)
        inheritance (begin
            (asserts! (is-eq tx-sender (get owner inheritance)) err-unauthorized)
            (asserts! (get active inheritance) err-inactive-inheritance)
            (asserts! (not (get claimed inheritance)) err-already-claimed)
            (asserts! (not (is-eq tx-sender new-beneficiary))
                err-invalid-beneficiary
            )

            (map-set inheritances inheritance-id
                (merge inheritance { beneficiary: new-beneficiary })
            )
            (ok true)
        )
        err-not-found
    )
)

(define-public (extend-unlock-time
        (inheritance-id uint)
        (additional-delay uint)
    )
    (match (get-inheritance inheritance-id)
        inheritance (begin
            (asserts! (is-eq tx-sender (get owner inheritance)) err-unauthorized)
            (asserts! (get active inheritance) err-inactive-inheritance)
            (asserts! (not (get claimed inheritance)) err-already-claimed)

            (let ((new-unlock-height (+ (get unlock-height inheritance) additional-delay)))
                (map-set inheritances inheritance-id
                    (merge inheritance { unlock-height: new-unlock-height })
                )
                (ok new-unlock-height)
            )
        )
        err-not-found
    )
)

(define-public (claim-inheritance (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (begin
            (asserts! (is-eq tx-sender (get beneficiary inheritance))
                err-unauthorized
            )
            (asserts! (get active inheritance) err-inactive-inheritance)
            (asserts! (not (get claimed inheritance)) err-already-claimed)
            (asserts! (>= stacks-block-height (get unlock-height inheritance))
                err-not-time-yet
            )

            (map-set inheritances inheritance-id
                (merge inheritance { claimed: true })
            )
            (ok true)
        )
        err-not-found
    )
)

(define-public (cancel-inheritance (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (begin
            (asserts! (is-eq tx-sender (get owner inheritance)) err-unauthorized)
            (asserts! (get active inheritance) err-inactive-inheritance)
            (asserts! (not (get claimed inheritance)) err-already-claimed)

            (map-delete nft-inheritance-map {
                contract: (get nft-contract inheritance),
                token-id: (get token-id inheritance),
            })
            (map-set inheritances inheritance-id
                (merge inheritance { active: false })
            )
            (ok true)
        )
        err-not-found
    )
)

(define-public (emergency-cancel (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (begin
            (asserts! (is-eq tx-sender contract-owner) err-owner-only)

            (map-delete nft-inheritance-map {
                contract: (get nft-contract inheritance),
                token-id: (get token-id inheritance),
            })
            (map-set inheritances inheritance-id
                (merge inheritance { active: false })
            )
            (ok true)
        )
        err-not-found
    )
)

(define-read-only (get-inheritance-details (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (ok {
            id: inheritance-id,
            owner: (get owner inheritance),
            beneficiary: (get beneficiary inheritance),
            nft-contract: (get nft-contract inheritance),
            token-id: (get token-id inheritance),
            unlock-height: (get unlock-height inheritance),
            blocks-remaining: (if (>= stacks-block-height (get unlock-height inheritance))
                u0
                (- (get unlock-height inheritance) stacks-block-height)
            ),
            claimed: (get claimed inheritance),
            active: (get active inheritance),
            created-at: (get created-at inheritance),
            status: (get-inheritance-status inheritance-id),
        })
        err-not-found
    )
)

(define-read-only (check-inheritance-validity
        (nft-contract principal)
        (token-id uint)
    )
    (match (get-inheritance-by-nft nft-contract token-id)
        inheritance (ok {
            exists: true,
            inheritance-id: (unwrap-panic (map-get? nft-inheritance-map {
                contract: nft-contract,
                token-id: token-id,
            })),
            owner: (get owner inheritance),
            beneficiary: (get beneficiary inheritance),
            active: (get active inheritance),
            claimed: (get claimed inheritance),
        })
        (ok {
            exists: false,
            inheritance-id: u0,
            owner: contract-owner,
            beneficiary: contract-owner,
            active: false,
            claimed: false,
        })
    )
)

(define-read-only (get-contract-stats)
    (ok {
        total-inheritances: (var-get inheritance-counter),
        current-block: stacks-block-height,
        contract-owner: contract-owner,
    })
)

(define-read-only (get-inheritance-info (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (ok inheritance)
        err-not-found
    )
)

(define-read-only (is-inheritance-active (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (get active inheritance)
        false
    )
)

(define-read-only (get-unlock-height (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (ok (get unlock-height inheritance))
        err-not-found
    )
)

(define-read-only (get-inheritance-owner (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (ok (get owner inheritance))
        err-not-found
    )
)

(define-read-only (get-inheritance-beneficiary (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (ok (get beneficiary inheritance))
        err-not-found
    )
)

(define-read-only (is-ready-to-claim (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (and
            (get active inheritance)
            (not (get claimed inheritance))
            (>= stacks-block-height (get unlock-height inheritance))
        )
        false
    )
)

(define-read-only (blocks-until-unlock (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (if (>= stacks-block-height (get unlock-height inheritance))
            u0
            (- (get unlock-height inheritance) stacks-block-height)
        )
        u0
    )
)
