#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>

#define static_assert _Static_assert
#define MALLOC(v) malloc(v)

typedef void* (*generic_func)();

typedef union _ocaml_t {
    int64_t i;
    double* f;
    char* str;
    union _ocaml_t* block;
    generic_func func;
} ocaml_t;

static_assert(sizeof(void*) == 8, "pointers must be 8 bytes");
static_assert(sizeof(double) == 8, "double must be 8 bytes");

#define GET_INT(v) ((int64_t)((v).i >> 1))
#define GET_FLOAT(v) ((v).f)
#define GET_STRING(v) ((v).str)
#define GET_BLOCK(v) ((v).block)
#define GET_FUNC(v) ((v).func)

#define BOX_INT(v) ((ocaml_t){.i = (int64_t)(v) << 1 | 1})
static inline ocaml_t BOX_FLOAT(double f) {
    double* fp = MALLOC(sizeof(double));
    *fp = f;
    return (ocaml_t){.f = fp};
}
#define BOX_STRING(v) ((ocaml_t){.str = (v)})
#define BOX_BLOCK(v) ((ocaml_t){.block = (v)})
#define BOX_FUNC(v) ((ocaml_t){.f = (generic_func)(v)})

#define IS_INT(v) ((v).i & 1)

void* print_string(char* string) {
    printf("%s", string);
    return 0;
}

void* print_int(int n) {
    printf("%d", n);
    return 0;
}

void* newline(void* x) {
    printf("\n");
    return 0;
}
