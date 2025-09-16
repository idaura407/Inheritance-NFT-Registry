# 🏛️ Inheritance NFT Registry

A Clarity smart contract for managing NFT inheritance on the Stacks blockchain. This contract allows NFT owners to designate beneficiaries who can claim their digital assets after a specified time period.

## 🌟 Features

- 📝 **Create Inheritance Plans** - Set up inheritance for any NFT with custom unlock delays
- 👥 **Beneficiary Management** - Update beneficiaries for existing inheritance plans
- ⏰ **Time-Based Unlocking** - Inheritances unlock after specified block heights
- 🔒 **Secure Claims** - Only designated beneficiaries can claim unlocked inheritances
- 🛡️ **Emergency Controls** - Contract owner can cancel inheritances if needed
- 📊 **Status Tracking** - Query inheritance status and details

## 🚀 Quick Start

### Prerequisites

- [Clarinet](https://docs.hiro.so/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository:
```bash
git clone https://github.com/idaura407/Inheritance-NFT-Registry.git
cd Inheritance-NFT-Registry
```

2. Check the contract:
```bash
clarinet check
```

3. Run tests:
```bash
npm install
npm test
```

## 📖 Usage

### Creating an Inheritance

Create an inheritance plan for your NFT:

```clarity
(contract-call? .Inheritance-NFT-Registry create-inheritance 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7.example-nft  ; NFT contract
  u1                                                        ; Token ID
  'SP1ABC123...                                             ; Beneficiary address
  u52560)                                                   ; Unlock delay (blocks)
```

### Claiming an Inheritance

Beneficiaries can claim unlocked inheritances:

```clarity
(contract-call? .Inheritance-NFT-Registry claim-inheritance u1)
```

### Checking Inheritance Status

```clarity
(contract-call? .Inheritance-NFT-Registry get-inheritance-details u1)
(contract-call? .Inheritance-NFT-Registry is-inheritance-unlocked u1)
(contract-call? .Inheritance-NFT-Registry blocks-until-unlock u1)
```

## 🔧 Core Functions

### Public Functions

| Function | Description |
|----------|-------------|
| `create-inheritance` | 📝 Create new inheritance plan |
| `update-beneficiary` | 👥 Change inheritance beneficiary |
| `extend-unlock-time` | ⏱️ Extend unlock delay |
| `claim-inheritance` | 🎁 Claim unlocked inheritance |
| `cancel-inheritance` | ❌ Cancel inheritance (owner only) |
| `emergency-cancel` | 🚨 Emergency cancellation (contract owner) |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-inheritance` | 📋 Get inheritance data |
| `get-inheritance-details` | 📊 Get detailed inheritance info |
| `get-inheritance-status` | 🔍 Get status string |
| `is-inheritance-unlocked` | 🔓 Check if inheritance is claimable |
| `check-inheritance-validity` | ✅ Validate inheritance exists |
| `get-contract-stats` | 📈 Get contract statistics |

## 🏗️ Contract Architecture

The contract uses several data structures:

- **inheritances**: Main inheritance data mapped by ID
- **nft-inheritance-map**: Maps NFT contracts + token IDs to inheritance IDs

### Inheritance Statuses

- 🔒 **locked** - Not yet unlocked
- 🔓 **unlocked** - Ready to claim
- ✅ **claimed** - Successfully claimed
- ❌ **inactive** - Cancelled or inactive

## 🛡️ Security Features

- ✋ Prevents self-inheritance (owner cannot be beneficiary)
- 🔐 Only beneficiaries can claim inheritances
- ⏰ Time-lock mechanism prevents premature claims
- 🚨 Emergency cancellation for contract owner
- 🛡️ Duplicate prevention for NFT inheritance plans

## 📝 Error Codes

| Code | Description |
|------|-------------|
| u100 | Owner only operation |
| u101 | Inheritance not found |
| u102 | Already claimed |
| u103 | Not time yet |
| u104 | Unauthorized |
| u105 | Invalid beneficiary |
| u106 | Already exists |
| u107 | Inactive inheritance |

## 🧪 Testing

Run the test suite:

```bash
npm test
```

## 📄 License

MIT License - see LICENSE file for details.



*Built with ❤️ using Clarity on Stacks*
