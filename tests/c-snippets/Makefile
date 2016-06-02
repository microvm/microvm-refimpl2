MU_INCLUDE = ../../cbinding

.PHONY: all
all: structtest.so callbacktest.so ncs_tests.so

structtest.so: structtest.c
	$(CC) -std=c11 -fPIC -shared -o structtest.so structtest.c

callbacktest.so: callbacktest.c
	$(CC) -std=c11 -fPIC -shared -o callbacktest.so callbacktest.c

ncs_tests.so: ncs_tests.c
	$(CC) -std=c11 -fPIC -shared -I $(MU_INCLUDE) -o ncs_tests.so ncs_tests.c

.PHONY: clean
clean:
	rm *.so
