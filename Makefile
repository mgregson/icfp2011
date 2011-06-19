CSCFLAGS = -O2

CSC = csc $(CSCFLAGS)

TARGETS = proto echobot

PROTO_OBJECTS = src/proto.o src/ltg-cards.o src/ltg-stack.o src/runner.o

all: $(TARGETS)

proto: $(PROTO_OBJECTS)
	$(CSC) -o $@ $^

echobot: src/echobot.o
	$(CSC) -o $@ $^

clean:
	rm -f $(TARGETS)
	rm -f $(PROTO_OBJECTS)

%.o: %.ss
	$(CSC) -c $<

%.o: %.scm
	$(CSC) -c $<

.SILENT:
