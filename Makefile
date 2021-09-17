sources = $(wildcard src/*.hs)
objects = $(patsubst src/%.hs,obj/%.o,$(sources))

GHC = ghc
PREFIX ?= /usr/local

all: haskalc

haskalc: $(objects)
	$(GHC) -o $@ $(objects)

obj/%.o: src/%.hs
	@mkdir -p obj
	$(GHC) -c -o $@ $<
	@rm -f $(patsubst src/%.hs,src/%.hi,$<)

clean:
	rm -rf obj
	rm -f haskalc

install: haskalc
	install -Dm755 haskalc $(PREFIX)/bin/haskalc

.PHONY: all clean install

