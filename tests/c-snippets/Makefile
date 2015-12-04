MU_INCLUDE = ../../cbinding

.PHONY: all
all: structtest.so callbacktest.so ncs_tests.so

structtest.so: structtest.c
	cc -std=c11 -fPIC -shared -o structtest.so structtest.c

callbacktest.so: callbacktest.c
	cc -std=c11 -fPIC -shared -o callbacktest.so callbacktest.c

ncs_tests.so: ncs_tests.c
	cc -std=c11 -fPIC -shared -I $(MU_INCLUDE) -o ncs_tests.so ncs_tests.c
