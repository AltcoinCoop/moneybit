#! /bin/bash

stack build --no-system-ghc --install-ghc --force-dirty \
  && cp .stack-work/install/x86_64-linux/nightly-2016-05-20/7.10.3/bin/moneybit bin/linux-x86_64/moneybit
