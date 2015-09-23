#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

typedef double (*DoubleToDouble)(double value);

double one_level(double v1, DoubleToDouble cb) {
    double v2, v3;
    v2 = cb(v1);
    v3 = cb(v1+1.0);
    return v2 + v3;
}


//typedef int (*PingPong)(int v, PingPong cb);  // ERROR
typedef void (*AnyFunc)();                      // workaround
typedef int (*PingPong)(int v, AnyFunc cb);   // workaround

int ping(int v, PingPong cb) {
    printf("[C:ping] v=%d, cb=%p\n", v, cb);
    if (v == 0) {
        return 1;
    } else {
        PingPong cbPingPong = (PingPong)cb;
        AnyFunc selfAnyFunc = (AnyFunc)ping;
        return v * cbPingPong(v-1, selfAnyFunc);
    }
}

typedef int (*TakeFromMu)();
typedef void (*GiveToMu)(int v);

void giver(GiveToMu cb) {
    int i;
    for (i=1;i<=10;i++) {
        printf("[c:giver] Sending %d...\n", i);
        cb(i);
        printf("[c:giver] Sent %d.\n", i);
    }
}

int taker(TakeFromMu cb) {
    int p = 1;
    while(1) {
        printf("[C:taker] p=%d. Receiving...\n", p);
        int v = cb();
        printf("[C:taker] Received value %d.\n", v);
        if (v < 0) {
            break;
        }
        p *= v;
    }
    printf("[C:taker] Returning value %d.\n", p);
    return p;
}
