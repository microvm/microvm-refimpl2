CFLAGS += -std=gnu11

ifndef JAVA_HOME
    $(error JAVA_HOME is required. Invoke with 'make JAVA_HOME=/path/to/java/home')
endif

CFLAGS += -I $(JAVA_HOME)/include

ifndef OS
    uname := $(shell uname)
    ifeq ($(uname),Darwin)
	OS = OSX
    else
    ifeq ($(uname),Linux)
	OS = LINUX
    else
	$(error Unrecognized operating system $(uname). I currently only worked on OSX and Linux.)
    endif
    endif
endif

ifeq ($(OS),OSX)
    CFLAGS += -I $(JAVA_HOME)/include/darwin
endif

ifeq ($(OS),LINUX)
    CFLAGS += -I $(JAVA_HOME)/include/linux
endif

LDFLAGS += -L $(JAVA_HOME)/jre/lib/server -l jvm -rpath $(JAVA_HOME)/jre/lib/server

.PHONY: all
all: libs tests

.PHONY: libs
libs: libmurefimpl2start.so

libmurefimpl2start.so: refimpl2-start.c classpath.h
	$(CC) -fPIC -shared $(CFLAGS) -o $@ $< $(LDFLAGS)

classpath.txt: ../build.sbt
	cd .. ; sbt makeClasspathFile

classpath.h: classpath.txt
	xxd -i classpath.txt > classpath.h

.PHONY: tests
tests: test_client

test_client: test_client.c libmurefimpl2start.so
	$(CC) $(CFLAGS) -I . -o $@ $< -L . -l murefimpl2start

.PHONY: clean veryclean

clean:
	rm *.so test_client

veryclean:
	rm *.so test_client classpath.txt classpath.h
