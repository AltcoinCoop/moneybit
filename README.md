# MoneyBit Monero Wallet

## WARNING -----------------------------------------

This app isn't functional, nor has it been well tested or audited for security
vulnerabilities. __Use at your own risk!__


-------------

> __Donation address__:
> `48iZ4NPuYsTfZEiYYXzKbTeZotimqEsfUB2LgykPAksdHkz4daHT46ZFsnkwRygxu2KR3KmkhpLvNQMtszjC3TsVFMLSNwK`


![](https://cdn.rawgit.com/moneybit/middleend/master/demo.gif)

(full video [here](http://webm.land/media/jKMC.webm))


## Running Client

> These steps _"should"_ work, but may not. If they don't, please file an
> [issue](https://github.com/moneybit/moneybit/issues)! If possible,
> make it an [SSCCE](http://sscce.org/) as well.

### Linux

- make sure `monero-wallet-cli` is in your PATH, or supply it manually
  in the `~/.moneybit/config.json` file.
- make sure libsodium.so.18 is available for your dynamic linker - this can be
  solved with (after install libsodium 1.0.11) `export LD_LINKER_PATH=/lib:/usr/lib:/usr/local/lib`


#### Running the Web Server

```bash
./bin/moneybit
```

This will open a port on `http://localhost:3000` - point your browser to that URL and
you're all set.

#### Running the Electron App (requires node.js)

```bash
npm install
./moneybit
```

### Windows

There's a few things you have to do before you can get MoneyBit working on Windows:

You need to have [monero-wallet-cli](https://getmonero.org) downloaded and "installed".
For MoneyBit, that just means you need to have `monero-wallet-cli.exe` accessible
globally. The easiest way to do this is copy `monero-wallet-cli.exe` to the same folder
that `moneybit.exe` resides in.

Download the [Windows release](https://github.com/moneybit/moneybit/releases/tag/v0.0.1-alpha) and extract it.
Clicking `moneybit.exe` will start a console, which means you can view `http://localhost:3000` in your
browser to use the wallet.


--------------------


## Building

### Linux

Steps:

- install [git](https://git-scm.com/)
- install [stack](https://www.haskellstack.org/)
- install [node](https://nodejs.org)
- install [bower](https://bower.io)
- install [libsodium](https://download.libsodium.org/doc/)
- clone this repo and the sub-repos
- fetch the assets for the frontend
- build the server (takes like 10 minutes)

#### Ubuntu

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

### Windows

- get [git and bash](https://git-scm.com/download/win)
- get [stack](https://haskellstack.org)
- get [nvm for windows](https://github.com/coreybutler/nvm-windows)
- get [libsodium 1.0.11 for mingw](https://download.libsodium.org/libsodium/releases/)
  and extract it somewhere

1. Clone moneybit and its submodules:

```bash
git clone git://github.com/moneybit/moneybit
cd moneybit/
git submodule update --init --recursive
```

2. Fetch the bower dependencies for the front end:

```bash
cd frontend/
bower install
cd ../ # in the main moneybit/ directory now
```

3. Build the server (may take a while)

```bash
stack build --extra-include-dirs <directory for libsodium>/include
  --stack-yaml stack-windows.yaml --install-ghc --no-system-ghc
```

This should fetch the compiler and build the whole shebang. Do note the `<dir...>`
portion - supply your own location for that. For instance:

```bash
$ stack build --extra-include-dirs /c/Users/athan/dev/libsodium-win64/include # ...
```
