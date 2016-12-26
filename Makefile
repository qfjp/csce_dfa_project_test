REMOVE=rm

MAKE=make

CC=gcc

LINK=gcc
OBJOPTS=-c


FENDIR=c_files
SRC=$(wildcard $(FENDIR)/*c)
OBJ=$(SRC:%.c=%.o)
INC=$(wildcard $(FENDIR)/*h)

ISDFA=$(FENDIR)/isDFA

export

all : isDFA cabal-all stack-all

clean : c-clean cabal-clean stack-clean

test : isDFA cabal-test stack-test

.PHONY : all clean test

include fenc.mk
include stack.mk
include cabal.mk
