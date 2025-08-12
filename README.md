# 🗳️ Blockchain Voter ID System

A decentralized voter identification system built on Stacks blockchain using Clarity smart contracts.

## 🎯 Features

- ✨ Secure voter ID issuance as NFTs
- 🔐 Identity verification system
- ⏰ Auto-expiration mechanism
- 🏠 Region-based registration
- 👮 Administrative controls
- 🚫 Duplicate prevention

## 🛠️ Usage

### For Voters

1. Get identity verified by contract owner
2. Register for voter ID with region code
3. Use voter ID for on-chain voting

### For Administrators

1. Verify citizen identities
2. Revoke compromised voter IDs
3. Toggle contract pause in emergencies

## 📝 Contract Functions

- `register-voter`: Issue new voter ID
- `verify-identity`: Verify citizen identity
- `revoke-voter-id`: Revoke existing voter ID
- `get-voter-record`: Query voter information
- `has-active-voter-id`: Check ID validity
- `toggle-contract-pause`: Emergency pause

## 🔧 Technical Details

- NFT-based voter IDs
- 1-year automatic expiration
- Region-code tracking
- Identity registry
- Duplicate prevention
```
