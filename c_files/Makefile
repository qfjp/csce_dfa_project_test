all: isDFA

isDFA: isDFA.o parser.o
	$(LINK) $(ISDFA_OUT) isDFA.o parser.o

isDFA.o: isDFA.c
	$(CC) $(OBJOPTS) isDFA.c

parser.o:
	$(CC) $(OBJOPTS) parser.c

parser.c: DFA.h hom.h

isDFA.c: DFA.h

DFA.h: globals.h

hom.h: globals.h

clean:
	$(REMOVE) *.o isDFA

.PHONY: clean
