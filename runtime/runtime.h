#include <stdlib.h>
#include <assert.h>
#include <stdint.h>

#define static_assert _Static_assert
#define MALLOC(v) malloc(v)

union _ocaml_t;

typedef struct {
    (void*)(*func)();
    intptr_t req_args;
    intptr_t curr_args;
    union _ocaml_t* args;
} closure_t;

typedef struct {
    intptr_t tag;
    union {
        double f;
        char* str;
        closure_t* closure;
        union _ocaml_t* block;
    } data;
} ocaml_block_t;

typedef union _ocaml_t {
    intptr_t i;
    ocaml_block_t* p;
} ocaml_t;

static_assert(sizeof(intptr_t) == 8, "intptr_t must be 8 bytes");
static_assert(sizeof(double) == 8, "double must be 8 bytes");

#define GET_INT(v) ((intptr_t)(t.i >> 1))
#define GET_FLOAT(v) ((v).p->data.f)
#define GET_STRING(v) ((v).p->data.str)
#define GET_BLOCK(v) ((v).p)

static inline ocaml_block_t* MAKE_BLOCK(intptr_t tag) {
    ocaml_block_t* block = MALLOC(sizeof(ocaml_block_t));
    block->tag = tag;
    return block;
}

#define BOX_INT(v) ((ocaml_t){.i = (v) << 1 | 1})
static inline ocaml_t BOX_FLOAT(double f) {
    ocaml_block_t* block = MAKE_BLOCK(0);
    block->data.f = f;
    return (ocaml_t){.p = block};
}
static inline ocaml_t BOX_STRING(char* str) {
    ocaml_block_t* block = MAKE_BLOCK(0);
    block->data.str = str;
    return (ocaml_t){.p = block};
}
#define BOX_BLOCK(v) ((ocaml_t){.p = (v)})

#define IS_INT(v) ((v).i & 1)
