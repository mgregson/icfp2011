CSCFLAGS = -O2

CSC = csc $(CSCFLAGS)

TARGETS = proto echobot

all: $(TARGETS)

proto: src/proto.scm
	$(CSC) -o $@ $<

echobot: src/echobot.ss
	$(CSC) -o $@ $<

clean:
	rm -f $(TARGETS)

.SILENT:
