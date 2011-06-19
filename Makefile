CSCFLAGS = -O2

CSC = csc $(CSCFLAGS)

TARGETS = debug echobot run holden mike combined

PROTO_OBJECTS = src/proto.o src/ltg-cards.o src/ltg-stack.o

TESTS = copy.test zombie.test zombieapply.test

all: $(TARGETS)

test: $(TESTS)

run: $(PROTO_OBJECTS) src/runner.o
	$(CSC) -o $@ $^

debug: $(PROTO_OBJECTS) src/debug.o
	$(CSC) -o $@ $^

holden: $(PROTO_OBJECTS) src/holden.o
	$(CSC) -o $@ $^

mike: $(PROTO_OBJECTS) src/mike.o
	$(CSC) -o $@ $^

combined: $(PROTO_OBJECTS) src/combined.o
	$(CSC) -o $@ $^

echobot: src/echobot.o
	$(CSC) -o $@ $^

clean:
	rm -f $(TARGETS)
	rm -f $(PROTO_OBJECTS)
	$(foreach i, $(TESTS), rm -f src/test/$(i); rm -f src/test/$(subst .test,.o,$i))

%.o: %.ss
	$(CSC) -c $<

%.o: %.scm
	$(CSC) -c $<

.SILENT:

.PHONY: all test $(TESTS)




#########  TESTS HERE #########

copy.test: $(PROTO_OBJECTS) src/test/copy.o
	$(CSC) -o src/test/$@ $^
	echo $(subst .test,,$@)":" `./src/test/$@`

zombie.test: $(PROTO_OBJECTS) src/test/zombie.o
	$(CSC) -o src/test/$@ $^
	echo $(subst .test,,$@)":" `./src/test/$@`

zombieapply.test: $(PROTO_OBJECTS) src/test/zombieapply.o
	$(CSC) -o src/test/$@ $^
	echo $(subst .test,,$@)":" `./src/test/$@`
