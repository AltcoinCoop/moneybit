# MoneyBit Monero Wallet

## Building

1. Get [stack](http://www.haskellstack.org/)
2. clone this repo & `cd` into it
3. `git submodule update --init --recursive` to get the frontend code
4. `./build` will compile the code & copy under `./bin/moneybit`
5. Run the server with `./bin/moneybit` (check out flags w/ `--help`)


## TODO

- frontend/backend encryption w/ libsodium - even though we're in localhost,
  an attacker could easily just prod the restufl interface to the wallets
- flesh out hmonero C bindings, or wait until `moner-wallet-cli` has UNIX socket
  support
