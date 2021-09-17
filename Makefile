sources = $(wildcard src/*.hs)

GHC = ghc -isrc -no-keep-hi-files -no-keep-o-files
PREFIX ?= /usr/local

# Disable if you don't have the readline package install
# See also: <https://hackage.haskell.org/package/readline>
ENABLE_READLINE ?= 1

ifeq ($(ENABLE_READLINE),1)
GHCFLAGS += -lreadline -DENABLE_READLINE=1
endif

all: haskalc

haskalc: $(sources)
	$(GHC) -o $@ $(sources) $(GHCFLAGS)

clean:
	rm -f haskalc

install: haskalc
	install -Dm755 haskalc $(PREFIX)/bin/haskalc

.PHONY: all clean install

