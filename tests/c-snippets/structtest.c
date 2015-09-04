#include <stdio.h>

struct foo {
    long a;
    int b;
    short c;
    char d;
};

struct bar {
    void *a;
    void (*b)();
};

struct baz {
    struct baz1 {
        float x;
        int y;
    } a;
    double b;
};

void foo_func(struct foo o, long *a, int *b, short *c, char *d) {
    *a = o.a;
    *b = o.b;
    *c = o.c;
    *d = o.d;
}

void bar_func(struct bar o, void **a, void (**b)()) {
    *a = o.a;
    *b = o.b;
}

struct baz baz_func(struct baz o, float *x, int *y, double *b, struct baz *extra) {
    *x = o.a.x;
    *y = o.a.y;
    *b = o.b;
    
    struct baz p;
    p.a.x = 4.0;
    p.a.y = 5;
    p.b = 6.0;

    printf("[C:baz_func] *extra = %p\n", extra);
    printf("[C:baz_func] extra->a.x = %f\n", extra->a.x);
    printf("[C:baz_func] extra->a.y = %d\n", extra->a.y);
    printf("[C:baz_func] extra->b = %lf\n", extra->b);

    *extra = p;

    printf("[C:baz_func] *extra = %p\n", extra);
    printf("[C:baz_func] extra->a.x = %f\n", extra->a.x);
    printf("[C:baz_func] extra->a.y = %d\n", extra->a.y);
    printf("[C:baz_func] extra->b = %lf\n", extra->b);


    return p;
}
