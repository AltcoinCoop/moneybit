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

1. You need to emulate POSIX. It's easiest to just [install git on Windows](https://git-scm.com/download/win). This will give you a BASH terminal (different from the normal DOS terminal), and a much-needed dependency - pthread.

2. You need to have [monero-wallet-cli](https://getmonero.org) downloaded and "installed".
   For MoneyBit, that just means you need to have `monero-wallet-cli.exe` accessible
   to your PATH environment variable. The easiest way to do this is run something like this:

```bash
export PATH=$PATH:<The the directory containing your executables>
```

For instance, if I had my monero executables in `C:\Users\foo\Downloads\monero\`, then
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


After this, it's fairly painless - just download the [Windows release](https://github.com/moneybit/moneybit/releases/tag/v0.0.1-alpha), extract the `.zip`, open a BASH
terminal, and invoke the executable:

```bash
foo@DESKTOP-NHAIOA7 MINGW64 ~
$ cd ~/Downloads/moneybit-windows-x86_64/

foo@DESKTOP-NHAIOA7 MINGW64 ~/Downloads/moneybit-windows-x86_64
$ ./moneybit.exe
```

You _may_ run into a couple of snags:

- if it complains `libsodium-18.dll` can't be found, make sure you either invoke
  `moneybit.exe` from the same directory that stores them (it's included in the release),
  or you can _try_ installing libsodium globally (but may void your warranty :x).
- it complaints `pthread` can't be found - if so, make sure you invoke moneybit from
  a _BASH_ terminal, not a _DOS_ terminal.



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
