MU_INCLUDE = ../../cinclude

.PHONY: all
all: structtest.so callbacktest.so ncs_tests.so

structtest.so: structtest.c
	cc -fPIC -shared -o structtest.so structtest.c

callbacktest.so: callbacktest.c
	cc -fPIC -shared -o callbacktest.so callbacktest.c

ncs_tests.so: ncs_tests.c
	cc -fPIC -shared -I $(MU_INCLUDE) -o ncs_tests.so ncs_tests.c
