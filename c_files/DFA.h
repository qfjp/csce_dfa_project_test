/* DFA.h - data structure for representing a DFA */
#ifndef DFA_H
#define DFA_H

#include <stdio.h>
#include "globals.h"

typedef struct 
{
    int num_states;
    int *accepting_states;
    int alpha_size;
    unsigned char *alphabet;
    int keymap[KEYMAP_SIZE];
    int *trans_tab;
} dfa_rec, *DFA;

int parseDFA(FILE *stream, DFA dfa);
void outputDFA(FILE *stream, DFA dfa);
int extended_trans(DFA dfa, int state, unsigned char *string);
void minimize(DFA dfa);
int isomorphic(FILE *stream, int strict, DFA ref_DFA, DFA in_DFA, int **isomorphism);

#endif
