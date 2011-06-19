CSCFLAGS = -O2

CSC = csc $(CSCFLAGS)

TARGETS = proto echobot

PROTO_OBJECTS = src/proto.o src/ltg-cards.o src/ltg-stack.o

TESTS = copy.test

all: $(TARGETS)

test: $(TESTS)

proto: $(PROTO_OBJECTS) src/runner.o
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

