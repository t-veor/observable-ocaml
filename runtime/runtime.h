#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <math.h>

#define static_assert _Static_assert
#define MALLOC(v) malloc(v)

union _variable_type;

typedef union value_type {
    intptr_t i;
    union _variable_type* block;
} value_type;

typedef void* (*generic_func)(void);

/* closures are represented as a stack, with the *closure representing the
 * current size of the stack */
typedef union _variable_type* closure_t;

typedef union _variable_type {
    intptr_t i;
    uintptr_t u;
    double fl;
    char* str;
    value_type value;
    closure_t closure;
    generic_func f;
    void* pointer;
} variable_type;

/* TODO */
typedef struct _closure_t_new {
    generic_func f;
    struct _closure_t_new* next;
    variable_type args[];
} closure_t_new;

static_assert(sizeof(intptr_t) == 8, "intptr_t must be 8 bytes");
static_assert(sizeof(uintptr_t) == 8, "uintptr_t must be 8 bytes");
static_assert(sizeof(void*) == 8, "pointers must be 8 bytes");
static_assert(sizeof(double) == 8, "double must be 8 bytes");
static_assert(sizeof(variable_type) == 8, "variable_type must be 8 bytes");
static_assert(sizeof(value_type) == 8, "value_type must be 8 bytes");

#define TO_INT(v) (((variable_type)v).i)
#define TO_UINT(v) (((variable_type)v).u)
#define TO_FLOAT(v) (((variable_type)v).fl)
#define TO_STR(v) (((variable_type)v).str)
#define TO_VALUE(v) (((variable_type)v).value)
#define TO_CLOSURE(v) (((variable_type)v).closure)
#define TO_FUNC(v) (((variable_type)v).f)

#define FROM_INT(v) ((variable_type){.i = (intptr_t)(v)})
#define FROM_UINT(v) ((variable_type){.u = (uintptr_t)(v)})
#define FROM_FLOAT(v) ((variable_type){.fl = (double)(v)})
#define FROM_STR(v) ((variable_type){.str = (char*)(v)})
#define FROM_VALUE(v) ((variable_type){.value = (value_type)(v)})
#define FROM_CLOSURE(v) ((variable_type){.closure = (closure_t)(v)})
#define FROM_FUNC(v) ((variable_type){.f = (generic_func)v})

#define UNBOX_INT(v) ((v).i >> 1)
#define UNBOX_BLOCK(v) ((v).block)
#define BOX_INT(v) ((value_type){.i = (intptr_t)(v) << 1 | 1})
#define BOX_BLOCK(v) ((value_type){.block = (variable_type*)(v)})
#define IS_INT(v) ((v).i & 1)

#define MEMCPY(s1, s2, n) do {\
    for (intptr_t i = 0; i < n; ++i) {\
        ((uintptr_t*)s1)[i] = ((uintptr_t*)s2)[i];\
    }\
} while(0)

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
