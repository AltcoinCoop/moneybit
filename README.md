# MoneyBit Monero Wallet


## Running Client (linux)


> This _"should"_ work, but may not. If it doesn't, please file an issue and copy
> the logs! If at all possible, __please__ try to make it an
> [SSCCE](http://sscce.org/).


```bash
npm install
./moneybit
```


## Building on linux

Steps:

- install [git](https://git-scm.com/)
- install [stack](https://www.haskellstack.org/)
- install [node](https://nodejs.org)
- install [bower](https://bower.io)
- install [libsodium](https://download.libsodium.org/doc/)
- clone this repo and the sub-repos
- fetch the assets for the frontend
- build the server (takes like 10 minutes)

### Ubuntu

__Get git and libsodium__:
```
sudo apt-get install git libsodium-dev
```

__Get haskell__:
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

it should be available as a command after that. Try `stack --version` just to check.

__Get node__:
```bash
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.32.0/install.sh | bash
nvm install node && nvm use node
```

__Get bower__:
```bash
npm install -g bower
```

__Cloning and fetching assets__:
```bash
git clone https://github.com/moneybit/middleend.git moneybit
cd moneybit/
git submodule update --init --recursive
npm install
cd frontend/
bower install
```

__Buiding__:
```bash
./build.sh
```

This will fetch the GHC compiler for Haskell and build the executable.
After that, you can run

```bash
./moneybit
```

to start the electrum client (soon to be integrated directly in the
haskell executable instead), of if you want, you can run the server
itself with `./bin/moneybit`. From there, you can point your browser
to `http://localhost:3000`.

## TODO / Needs to be implemented

- frontend/backend encryption w/ libsodium
    - Generate a shared private key at compile time, to ensure server
      authenticity (poor man's SSL, but still strong)
    - I'm steering away from a TLS layer, because that would imply a
      certificate. I could maintain my own with letsencrypt or something
      similar, but I want less ties to my consistency and more stability
      even through abandonment
- flesh out monero C bindings to Haskell (difficult/fun :D)
- More UX stuff
