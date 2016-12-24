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

all : Main isDFA

clean : c-clean hs-clean

test : isDFA hs-test

.PHONY : clean test

include fenc.mk
include haskell.mk

