# CSCE 355 Project: Haskell Tester
A Haskell program to test submissions for the CSCE 355 DFA Project

[![Build Status](https://travis-ci.org/qfjp/csce_dfa_project_test.svg?branch=master)](https://travis-ci.org/qfjp/csce_dfa_project_test/)
[![Coverage Status](https://coveralls.io/repos/github/qfjp/csce_dfa_project_test/badge.svg?branch=master)](https://coveralls.io/github/qfjp/csce_dfa_project_test?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/5cv1bhk8c24x6bht?svg=true)](https://ci.appveyor.com/project/qfjp/csce-dfa-project-test)

## Install on Lab Linux Machines (Cabal method)

Space is limited on the linux machines, so stack is not yet an option.
To install, type the following at the command prompt:

    make cabal-all

If you want to verify the executable by running the test suite, do the
following instead

    make cabal-test
    make cabal-all

## Install on Windows (Work in Progress)

First, ensure you have [haskell
stack](https://docs.haskellstack.org/en/stable/README/) and
[MinGW](http://mingw.org/) installed to the default locations. At a
command prompt, type the following:

    set PATH=%PATH%;C:\MinGW\bin
    mingw32-make.exe stack-all

You should verify the tests work before you run this on your own
program

    mingw32-make.exe stack-test

## Linux and Mac OS X

Ensure you have [haskell stack](https://docs.haskellstack.org/en/stable/README/)
installed. At a command prompt, type the following:

    make stack-all

You should verify the tests work before you run this on your own
program

    make stack-test
