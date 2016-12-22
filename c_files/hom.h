/* homo.h - data structure for representing a homomorphism between alphabets */
#ifndef HOMO_H
#define HOMO_H

#include <stdio.h>
#include "globals.h"

typedef struct 
{
    int in_asize, out_asize;
    unsigned char *in_alphabet, *out_alphabet;
    int in_keymap[KEYMAP_SIZE], out_keymap[KEYMAP_SIZE];
    unsigned char **map;
} homo_rec, *HOMO;

int parseHomomorphism(FILE *stream, HOMO h);

#endif
