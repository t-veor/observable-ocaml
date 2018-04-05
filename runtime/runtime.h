#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <math.h>
#include "gc.h"

#define static_assert _Static_assert
#define MALLOC(v) GC_MALLOC(v)

/* RUNTIME TYPE DECLARATIONS */

struct _block;

#ifdef DEBUG

typedef union _value_type {
    intptr_t i;
    struct _block* block;
} value_type;

#endif

typedef void* (*generic_func)();

struct _closure_type;

typedef union _any_type {
    intptr_t i;
    uintptr_t u;
    double fl;
    char* str;
#ifdef DEBUG
    value_type value;
#else
    struct _block* block;
#endif
    struct _closure_type* closure;
    generic_func f;
    void* pointer;
} any_type;

#ifndef DEBUG
typedef any_type value_type;
#endif

typedef struct _block{
    uintptr_t size : 56;
    uintptr_t tag : 8;
    any_type data[];
} block;

typedef struct _closure_type {
    generic_func f;
    struct _closure_type* next;
    any_type args[];
} closure_type;

/* TYPE ASSERTIONS */

static_assert(sizeof(intptr_t) == 8, "intptr_t must be 8 bytes");
static_assert(sizeof(uintptr_t) == 8, "uintptr_t must be 8 bytes");
static_assert(sizeof(void*) == 8, "pointers must be 8 bytes");
static_assert(sizeof(double) == 8, "double must be 8 bytes");
static_assert(sizeof(block) == 8, "block must be 8 bytes");
static_assert(sizeof(any_type) == 8, "any_type must be 8 bytes");
static_assert(sizeof(value_type) == 8, "value_type must be 8 bytes");
static_assert(sizeof(closure_type) % sizeof(any_type) == 0, "closure_type must be a multiple of any_type");

/* TAG DECLARATIONS */

#define CLOSURE_TAG 247
#define STRING_TAG 252
#define DOUBLE_TAG 253

/* PACKING SHORTHANDS TO AND FROM ANY_TYPE */

#define TO_INT(v) (((any_type)v).i)
#define TO_UINT(v) (((any_type)v).u)
#define TO_FLOAT(v) (((any_type)v).fl)
#define TO_STR(v) (((any_type)v).str)
#ifdef DEBUG
#define TO_VALUE(v) (((any_type)v).value)
#else
#define TO_VALUE(v) (v)
#endif
#define TO_CLOSURE(v) (((any_type)v).closure)
#define TO_FUNC(v) (((any_type)v).f)

#define FROM_INT(v) ((any_type){.i = (intptr_t)(v)})
#define FROM_UINT(v) ((any_type){.u = (uintptr_t)(v)})
#define FROM_FLOAT(v) ((any_type){.fl = (double)(v)})
#define FROM_STR(v) ((any_type){.str = (char*)(v)})
#ifdef DEBUG
#define FROM_VALUE(v) ((any_type){.value = (value_type)(v)})
#else
#define FROM_VALUE(v) (v)
#endif
#define FROM_CLOSURE(v) ((any_type){.closure = (closure_type*)(v)})
#define FROM_FUNC(v) ((any_type){.f = (generic_func)v})

/* BOXING SHORTHANDS TO AND FROM VALUE_TYPE */

#define UNBOX_INT(v) ((v).i >> 1)
#define UNBOX_UINT(v) ((v).i >> 1)

#ifdef DEBUG

#define UNBOX_FLOAT(v) ((v).block->data[0].fl)
#define UNBOX_STR(v) ((v).block->data[0].str)
#define UNBOX_CLOSURE(v) ((v).block->data[0].closure)

#else

#define UNBOX_FLOAT(v) TO_FLOAT(v)
#define UNBOX_STR(v) TO_STR(v)
#define UNBOX_CLOSURE(v) TO_CLOSURE(v)

#endif

#define UNBOX_BLOCK(v) ((v).block)

#define BOX_BLOCK(v) ((value_type){.block = (block*)(v)})
static inline value_type BOX_GEN(any_type val, unsigned int tag) {
    block* b = MALLOC(sizeof(block) + sizeof(any_type));
    b->size = 1;
    b->tag = tag;
    b->data[0] = val;
    return BOX_BLOCK(b);
}

#define BOX_INT(v) ((value_type){.i = (intptr_t)(v) << 1 | 1})
#define BOX_UINT(v) ((value_type){.i = (uintptr_t)(v) << 1 | 1})

#ifdef DEBUG

#define BOX_FLOAT(v) BOX_GEN(FROM_FLOAT(v), DOUBLE_TAG)
#define BOX_STR(v) BOX_GEN(FROM_STR(v), STRING_TAG)
#define BOX_CLOSURE(v) BOX_GEN(FROM_CLOSURE(v), CLOSURE_TAG)

#else

#define BOX_FLOAT(v) FROM_FLOAT(v)
#define BOX_STR(v) FROM_STR(v)
#define BOX_CLOSURE(v) FROM_CLOSURE(v)

#endif

/* UTILITY FUNCTIONS */

#define IS_INT(v) ((v).i & 1)

#define MEMCPY(s1, s2, n) do {\
    for (intptr_t i = 0; i < n; ++i) {\
        ((uintptr_t*)s1)[i] = ((uintptr_t*)s2)[i];\
    }\
} while(0)

/* RUNTIME IO FUNCTIONS */

value_type print_string(char* string) {
    printf("%s", string);
    return BOX_INT(0);
}

value_type print_int(intptr_t n) {
    printf("%d", (int)n);
    return BOX_INT(0);
}

value_type print_double(double n) {
    printf("%f", n);
    return BOX_INT(0);
}

value_type newline(value_type x) {
    printf("\n");
    return BOX_INT(0);
}

value_type print_byte(int x) {
    putchar(x);
    return BOX_INT(0);
}
