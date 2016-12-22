/* parser.c - Routines for parsing a DFA or homomorphism from an input stream */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "DFA.h"
#include "hom.h"

static unsigned char *get_alphabet(FILE *stream, int *alen, int *keymap);

typedef struct sti 
{
    unsigned char data;
    struct sti *next;
} char_stack_item, *CSTACK;

static void push(unsigned char c, CSTACK *stack)
{
    CSTACK new_item = (CSTACK)malloc(sizeof(char_stack_item));
    assert(new_item != NULL);
    new_item->data = c;
    new_item->next = *stack;
    *stack = new_item;
};

static int pop(CSTACK *stack)
{
    CSTACK old_item;
    int ret;
    if (*stack == NULL)
        return -1;
    old_item = *stack;
    *stack = (*stack)->next;
    ret = old_item->data;
    free(old_item);
    return ret;
}

// Reads through to the next occurrence of target;
// returns TRUE iff target not found
int read_to_char(FILE *stream, int target)
{
    int c;
    
    while ((c = getc(stream)) != EOF && c != target);
    if (c == EOF)
        fprintf(stderr, "Character '%c' not found\n", target);
    return (c == EOF);
}

int read_white_space(FILE *stream)
{
    int c;
    while ((c = getc(stream)) == ' ' || c == '\t');
    ungetc(c, stream);
    return c;
}

int eat_comments(FILE *stream)
{
    int c, ret=0;
    while ((c = getc(stream)) == '#') {
        ret = read_to_char(stream, '\n');
        if (ret) break;
    }
    if (c != EOF)
        ungetc(c, stream);
    return ret;
}
    

// Assumes h points to an existing struct
// Returns input error status (0 means no error)
int parseHomomorphism(FILE *stream, HOMO h)
{
    int i, hlen, c;
    unsigned char *buf;
    CSTACK s;
    
    if (eat_comments(stream))
        return 1;
    h->in_alphabet = get_alphabet(stream, &h->in_asize, h->in_keymap);
    if (h->in_alphabet == NULL || eat_comments(stream))
        return 1;
    h->out_alphabet = get_alphabet(stream, &h->out_asize, h->out_keymap);
    if (h->out_alphabet == NULL)
        return 1;

        // Read h(a) for all a in the input alphabet
    h->map = (unsigned char **)malloc(h->in_asize * sizeof(unsigned char *));
    assert(h->map != NULL);
    for (i=0; i<h->in_asize; i++) {
        s = NULL; hlen = 0;
        for (c=getc(stream); ; c=getc(stream)) {
            if (c == EOF) {
                fprintf(stderr, "EOF encountered reading homomorphism\n");
                return 1;
            }
            if (c<0 || c>=KEYMAP_SIZE) {
                fprintf(stderr, "Bad character (value %u)\n", c);
                return 1;
            }
            if (h->out_keymap[c] < 0) break;
            push(c, &s);
            hlen++;
        }
        if (c != '\n') {
            fprintf(stderr, "Illegal output alphabet symbol: '%c'\n", c);
            return 1;
        }
        buf = (unsigned char *)malloc((hlen+1)*sizeof(unsigned char));
        assert(buf != NULL);
        buf[hlen] = '\0';
        while (--hlen >= 0) {
            c = pop(&s);
            assert(c >= 0);
            buf[hlen] = c;
        }
        assert(s == NULL);
        #if DEBUG
        fprintf(stderr, "  Installing h(%c) = \"%s\"\n", h->in_alphabet[i], buf);
        #endif
        h->map[i] = buf;
    }

    return 0;
}


// Assumes dfa points to an existing struct
// Returns input error status (0 means no error)
int parseDFA(FILE *stream, DFA dfa)
{
    int c;
    int ns, *as, *ttab;
    int state;
    int i, alen, tlen;

    assert(dfa != NULL);

        // Get number of states
    if (eat_comments(stream))
        return 1;
    if (read_to_char(stream, ':'))
        return 1;
    if (fscanf(stream, "%d", &ns) != 1) {
        fprintf(stderr, "Malformed integer constant (number of states)\n");
        return 1;
    }
    if (ns <= 0) {
        fprintf(stderr, "Must have positive number of states\n");
        return 1;
    }
    dfa->num_states = ns;
    if (read_to_char(stream, '\n') || eat_comments(stream))
        return 1;

        // Get accepting states
    as = (int *)malloc(ns * sizeof(int));
    assert(as != NULL);
    if (read_to_char(stream, ':'))
        return 1;
    for (state=0; state<ns; state++)
        as[state] = 0;
    while (isdigit(read_white_space(stream))) {
        if (fscanf(stream, "%d", &state) != 1) {
            fprintf(stderr, "Malformed integer constant (accepting states)\n");
            return 1;
        }
        if (state < 0 || state >= ns) {
            fprintf(stderr, "State %d out of range\n", state);
            return 1;
        }
        as[state] = 1;
    }
    dfa->accepting_states = as;
    if (read_to_char(stream, '\n') || eat_comments(stream))
        return 1;

        // Get alphabet
        // fprintf(stderr, "Getting the alphabet ...");
    dfa->alphabet = get_alphabet(stream, &alen, dfa->keymap);
    if (dfa->alphabet == NULL || eat_comments(stream))
        return 1;
    dfa->alpha_size = alen;
        // fprintf(stderr, "done\n");

        // Get transition table
    tlen = alen * ns;
    ttab = (int *)malloc(tlen * sizeof(int));
    assert(ttab != NULL);
    for (i=0; i<tlen; i++) {
        if (fscanf(stream, "%d", &state) != 1) {
            fprintf(stderr, "Malformed integer constant (transition table)\n");
            return 1;
        }
        if (state < 0 || state >= ns) {
            fprintf(stderr, "State value out of range: %d\n", state);
            return 1;
        }
        ttab[i] = state;
    }
    dfa->trans_tab = ttab;
    if (read_to_char(stream, '\n') || eat_comments(stream))
        return 1;

        // Make sure there is nothing left over on the input except whitespace
    while ((c = getc(stream)) != EOF)
        if (c != ' ' && c != '\t' && c != '\n') {
            fprintf(stderr, "Extra nonblank character in file: '%c'\n", c);
            return 1;
        }

    return 0;
}

static unsigned char *get_alphabet(FILE *stream, int *alen, int *keymap)
{
    unsigned char buf[KEYMAP_SIZE+2], *b, *ret, c;
    int as, i;
    
    if (read_to_char(stream, ':'))
        return NULL;
    if (read_to_char(stream, ' '))
        return NULL;
    b = fgets(buf, KEYMAP_SIZE+2, stream);
    assert(b != NULL);
    as = strlen(buf);
    if (as <= 0 || buf[as-1] != '\n') {
        fprintf(stderr, "Bad alphabet size: %d\n", as);
        return NULL;
    }
    buf[--as] = '\0';
    if (as == 0) {
        fprintf(stderr, "Bad alphabet size: %d\n", as);        
        return NULL;
    }
        // Check if alphabet is in increasing order and set keymap
    for (i=0; i<KEYMAP_SIZE; i++)
        keymap[i] = -1;
        // Space is the first printable ASCII char, and tilde is the last
    c = ' ' - 1;
    for (i=0, b=buf; *b; i++, b++) {
        if (*b > '~' || *b <= c) {
            fprintf(stderr, "Bad or out-of-order alphabet symbol: '%c'\n", *b);
            return NULL;
        }
        c = *b;
        keymap[*b] = i;
    }
    ret = (unsigned char *)malloc((as+1) * sizeof(unsigned char));
    assert(ret != NULL);
    (void) strcpy(ret, buf);
    *alen = as;
    return ret;
}
