isDFA : $(OBJ)
	$(LINK) -o $(ISDFA) $(OBJ)

$(OBJ) : %.o : %.c $(INC)
	$(CC) $(OBJOPTS) -o $@ $<

c-clean :
	$(REMOVE) $(OBJ) $(ISDFA)

.PHONY: c-clean
