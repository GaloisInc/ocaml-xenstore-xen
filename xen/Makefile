# Define a BACKEND variable usable in sub-Makefiles

BACKEND := --xen

.PHONY: all clean

all: build
	@

src/dist/setup:
	mirari configure $(BACKEND)

build: src/dist/setup
	mirari build $(BACKEND)

run:
	mirari run $(BACKEND)

clean:
	mirari clean
