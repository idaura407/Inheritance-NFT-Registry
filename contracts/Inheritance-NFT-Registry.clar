(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-claimed (err u102))
(define-constant err-not-time-yet (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-beneficiary (err u105))
(define-constant err-already-exists (err u106))
(define-constant err-inactive-inheritance (err u107))
(define-constant err-guardian-not-found (err u108))
(define-constant err-already-approved (err u109))
(define-constant err-insufficient-approvals (err u110))
(define-constant err-invalid-guardian (err u111))
(define-constant err-max-guardians-exceeded (err u112))

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
        required-approvals: uint,
        current-approvals: uint,
    }
)

(define-map nft-inheritance-map
    {
        contract: principal,
        token-id: uint,
    }
    uint
)

(define-map inheritance-guardians
    {
        inheritance-id: uint,
        guardian: principal,
    }
    {
        approved: bool,
        approved-at: (optional uint),
    }
)

(define-map inheritance-guardian-list
    uint
    (list 10 principal)
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
            (>= (get current-approvals inheritance)
                (get required-approvals inheritance)
            )
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
                (if (and
                        (>= stacks-block-height (get unlock-height inheritance))
                        (>= (get current-approvals inheritance)
                            (get required-approvals inheritance)
                        )
                    )
                    "unlocked"
                    (if (>= stacks-block-height (get unlock-height inheritance))
                        "pending-approval"
                        "locked"
                    )
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
        (guardians (list 10 principal))
        (required-approvals uint)
    )
    (let (
            (inheritance-id (+ (var-get inheritance-counter) u1))
            (unlock-height (+ stacks-block-height unlock-delay))
            (guardian-count (len guardians))
        )
        (asserts! (not (is-eq tx-sender beneficiary)) err-invalid-beneficiary)
        (asserts! (<= guardian-count u10) err-max-guardians-exceeded)
        (asserts! (<= required-approvals guardian-count)
            err-insufficient-approvals
        )
        (asserts! (> required-approvals u0) err-insufficient-approvals)
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
            required-approvals: required-approvals,
            current-approvals: u0,
        })

        (map-set inheritance-guardian-list inheritance-id guardians)

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
            required-approvals: (get required-approvals inheritance),
            current-approvals: (get current-approvals inheritance),
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
            (>= (get current-approvals inheritance)
                (get required-approvals inheritance)
            )
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

(define-public (approve-inheritance (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (begin
            (asserts! (get active inheritance) err-inactive-inheritance)
            (asserts! (not (get claimed inheritance)) err-already-claimed)

            (let (
                    (guardian-key {
                        inheritance-id: inheritance-id,
                        guardian: tx-sender,
                    })
                    (current-approval (map-get? inheritance-guardians guardian-key))
                    (guardians-list (default-to (list)
                        (map-get? inheritance-guardian-list inheritance-id)
                    ))
                )
                (asserts! (is-some (index-of guardians-list tx-sender))
                    err-unauthorized
                )
                (asserts! (is-none current-approval) err-already-approved)

                (map-set inheritance-guardians guardian-key {
                    approved: true,
                    approved-at: (some stacks-block-height),
                })

                (let ((new-approval-count (+ (get current-approvals inheritance) u1)))
                    (map-set inheritances inheritance-id
                        (merge inheritance { current-approvals: new-approval-count })
                    )
                    (ok new-approval-count)
                )
            )
        )
        err-not-found
    )
)

(define-public (revoke-approval (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (begin
            (asserts! (get active inheritance) err-inactive-inheritance)
            (asserts! (not (get claimed inheritance)) err-already-claimed)

            (let (
                    (guardian-key {
                        inheritance-id: inheritance-id,
                        guardian: tx-sender,
                    })
                    (current-approval (map-get? inheritance-guardians guardian-key))
                    (guardians-list (default-to (list)
                        (map-get? inheritance-guardian-list inheritance-id)
                    ))
                )
                (asserts! (is-some (index-of guardians-list tx-sender))
                    err-unauthorized
                )
                (asserts! (is-some current-approval) err-guardian-not-found)

                (map-delete inheritance-guardians guardian-key)

                (let ((new-approval-count (- (get current-approvals inheritance) u1)))
                    (map-set inheritances inheritance-id
                        (merge inheritance { current-approvals: new-approval-count })
                    )
                    (ok new-approval-count)
                )
            )
        )
        err-not-found
    )
)

(define-read-only (get-guardian-approval
        (inheritance-id uint)
        (guardian principal)
    )
    (map-get? inheritance-guardians {
        inheritance-id: inheritance-id,
        guardian: guardian,
    })
)

(define-read-only (get-inheritance-guardians (inheritance-id uint))
    (map-get? inheritance-guardian-list inheritance-id)
)

(define-read-only (is-guardian
        (inheritance-id uint)
        (guardian principal)
    )
    (let ((guardians-list (default-to (list) (map-get? inheritance-guardian-list inheritance-id))))
        (is-some (index-of guardians-list guardian))
    )
)

(define-read-only (get-approval-progress (inheritance-id uint))
    (match (get-inheritance inheritance-id)
        inheritance (ok {
            current-approvals: (get current-approvals inheritance),
            required-approvals: (get required-approvals inheritance),
            is-fully-approved: (>= (get current-approvals inheritance)
                (get required-approvals inheritance)
            ),
        })
        err-not-found
    )
)
