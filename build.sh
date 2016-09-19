#! /bin/bash

stack build --no-system-ghc --install-ghc \
  && cp .stack-work/install/x86_64-linux/nightly-2016-05-20/7.10.3/bin/backend bin/backend
