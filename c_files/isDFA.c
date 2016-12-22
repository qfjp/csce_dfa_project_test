/* isDFA.c - test if file on stdin is a correct DFA; if so give no. of states */

#include "DFA.h"

dfa_rec the_DFA;


int main()
{
    int err;

    err = parseDFA(stdin, &the_DFA);
    if (err)
        printf("Not a DFA\n");
    else
        printf("A DFA with %d states and alphabet %s\n",
               the_DFA.num_states, the_DFA.alphabet);
    return 0;
}
