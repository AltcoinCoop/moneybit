#! /bin/bash

cd frontend/ \
    && git pull local master \
    && bower install \
    && cd ..

git submodule update --recursive --init
