/*
copyright paxdiablo
lifted directly from: http://powerfield-software.com/?p=615
*/

#ifndef _PFHASH_H
#define _PFHASH_H

    #include <R.h>
    #include <Rinternals.h>
    #include <ctype.h>
    #include <stdint.h>

    typedef struct sPfHashNode {
        char *key;
        char *data;
        struct sPfHashNode *next;
    } pfHashNode;

    typedef struct {
        uint32_t (*fn) (char *);
        pfHashNode *lookup[];
    } pfHashTable;

    pfHashTable *pfHashCreate (uint32_t(*)(char*));
    void pfHashDestroy (pfHashTable*);
    int pfHashSet (pfHashTable*,char*,char*);
    int pfHashDel (pfHashTable*,char*);
    char *pfHashFind (pfHashTable*,char*);
    void pfHashDebug (pfHashTable*,char*);

#endif
