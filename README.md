# MoneyBit Monero Wallet

## WARNING

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

```bash
npm install
./moneybit
```

### Windows

in a bash terminal, make sure the folder containing the `monero-wallet-cli.exe` file
is listed between the colon `:` characters. if it's not, add it with the following
line:

```bash
export PATH=$PATH:<The the directory containing your executables>
```

So, if I had my monero executables in `C:\Users\foo\Downloads\monero\`, then
you add it like this:

```bash
foo@DESKTOP-NHAIOA7 MINGW64 ~
$ export PATH=$PATH:/c/Users/foo/Downloads/monoero

foo@DESKTOP-NAHIOA7 MINGW64 ~
$ 
```

> __Note__: in UNIX, parent/child separation is done with a forward slash, like URLs,
> while in normal Windows, backslashes `\` are used. Also, in unis there is _always_ a
> "root" directory `/` - so instead of `C:\..` you have `/c/..` - hence the leftmost
> forward slash.

#### Node.js

Next, you have to get node.js up and running. After you install nvm, you'll be able
to install it with:

```bash
nvm install 6.7.0
```

after it does it's thing, load it with:

```bash
nvm use 6.7.0
```

Next, install `bower` globally:

```bash
npm install -g bower
```

### LibSodium

There's luckily

> _Coming Soon_ [tm]


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
- 




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
