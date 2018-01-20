#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>

#define static_assert _Static_assert
#define MALLOC(v) malloc(v)

typedef void* variable_type;

/* closures are represented as a stack, with the *closure representing the
 * current size of the stack */
typedef variable_type* closure_t;

typedef void* (*generic_func)();

typedef union _ocaml_t {
    int64_t i;
    double* f;
    char* str;
    variable_type* block;
    closure_t closure;
    generic_func func;
} ocaml_t;

static_assert(sizeof(void*) == 8, "pointers must be 8 bytes");
static_assert(sizeof(double) == 8, "double must be 8 bytes");

#define GET_INT(v) ((int64_t)((v).i >> 1))
#define GET_FLOAT(v) ((v).f)
#define GET_STRING(v) ((v).str)
#define GET_BLOCK(v) ((v).block)
#define GET_FUNC(v) ((v).func)
#define GET_CLOSURE(v) ((v).closure)

#define BOX_INT(v) ((ocaml_t){.i = (int64_t)(v) << 1 | 1})
static inline ocaml_t BOX_FLOAT(double f) {
    double* fp = MALLOC(sizeof(double));
    *fp = f;
    return (ocaml_t){.f = fp};
}
#define BOX_STRING(v) ((ocaml_t){.str = (v)})
#define BOX_BLOCK(v) ((ocaml_t){.block = (v)})
#define BOX_FUNC(v) ((ocaml_t){.func = (generic_func)(v)})
#define BOX_CLOSURE(v) ((ocaml_t){.closure = (v)})

#define IS_INT(v) ((v).i & 1)

static inline void MEMCPY(void* s1, const void* s2, int64_t n) {
    for (int64_t i = 0; i < n; ++i) {
        ((uint64_t*)s1)[i] = ((uint64_t*)s2)[i];
    }
}

void* print_string(char* string) {
    printf("%s", string);
    return 0;
}

void* print_int(int n) {
    printf("%d", n);
    return 0;
}

void* print_double(double n) {
    printf("%f", n);
    return 0;
}

void* newline(void* x) {
    printf("\n");
    return 0;
}
