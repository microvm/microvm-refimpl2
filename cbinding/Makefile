CFLAGS += -std=gnu11
ifndef JAVA_HOME
    $(error JAVA_HOME is required. Invoke with 'make JAVA_HOME=/path/to/java/home')
endif

.PHONY: all
all: libmurefimpl2start.so

libmurefimpl2start.so: refimpl2-start.c cbinding.txt
	$(CC) -fPIC -shared $(CFLAGS) -o $@ $< $(LDFLAGS)

cbinding.txt: ../build.sbt
	cd .. ; sbt makeJarListFile

.PHONY: clean

clean:
	rm *.so
