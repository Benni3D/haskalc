sources = $(wildcard src/*.hs)

GHC ?= ghc
GHCFLAGS += -isrc -no-keep-hi-files -no-keep-o-files

PREFIX ?= /usr/local

# Disable if you don't have the `readline` package installed
# See also: <https://hackage.haskell.org/package/readline>
ENABLE_READLINE ?= 1

# Disable if you don't have the `unix` package installed
# See also: <https://hackage.haskell.org/package/unix>
ENABLE_POSIX ?= 0				# experimental

ifeq ($(ENABLE_READLINE),1)
GHCFLAGS += -lreadline -DENABLE_READLINE=1
endif

ifeq ($(ENABLE_POSIX),1)
GHCFLAGS += -DENABLE_POSIX=1
endif

all: haskalc

haskalc: $(sources)
	$(GHC) -o $@ $(sources) $(GHCFLAGS)

clean:
	rm -f haskalc

install: haskalc
	install -Dm755 haskalc $(PREFIX)/bin/haskalc
	install -Dm644 haskalc.1 $(PREFIX)/share/man/man1/haskalc.1

.PHONY: all clean install

