#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

double (*DoubleToDouble)(double value);

double one_level(double v1, DoubleToDouble cb) {
    double v2, v3;
    v2 = cb(v1+1.0);
    v3 = cb(v1+2.0);
    return v2 + v3;
}
