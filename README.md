# MoneyBit Monero Wallet


## Running Client (linux)


This _should_ work. If it doesn't, please file an issue.


```bash
npm install
./moneybit
```


## Building

> 0. Use linux or mac :b

1. Get [stack](http://www.haskellstack.org/), a recent version of node, and bower
2. clone this repo & `cd` into it
3. `git submodule update --init --recursive` to get the frontend code
3.5 `cd frontend && bower install && cd ../` to fetch assets like jQuery
4. `./build.sh` will compile the code & copy under `./bin/moneybit`
5. Run the server with `./bin/moneybit` (check out the flags w/ `--help`)


Then, you can point your browser to `http://localhost:3000` to see it in
action.


## TODO

- electron/webkit wrapper of the web page, to make it "feel" like an application
- frontend/backend encryption w/ libsodium - even though we're in localhost,
  an attacker could easily just prod the restufl interface to the wallets
- flesh out hmonero C bindings, or wait until `moner-wallet-cli` has UNIX socket
  support
