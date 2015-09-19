all: structtest.so callbacktest.so

structtest.so: structtest.c
	cc -fPIC -shared -o structtest.so structtest.c

callbacktest.so: callbacktest.c
	cc -fPIC -shared -o callbacktest.so callbacktest.c
