sources = $(wildcard src/*.hs)

GHC = ghc -isrc -no-keep-hi-files -no-keep-o-files
PREFIX ?= /usr/local

all: haskalc

haskalc: $(sources)
	$(GHC) -o $@ $(sources)

clean:
	rm -f haskalc

install: haskalc
	install -Dm755 haskalc $(PREFIX)/bin/haskalc

.PHONY: all clean install

