#!/usr/bin/env bash

ls src/**/*.hs | entr -c cabal run library-tests
