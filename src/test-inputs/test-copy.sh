#!/usr/bin/env bash
RESULT=`echo "(load \"../proto.scm\")
(set! players (player-field! (player-field! players 0 0 (card-function zero)) 1 255 (make-stack-item \"255\" val 0 0)))
(main '(\"t\"))
1
copy
255
" | csi -quiet 2> /dev/null | tail -n 1`
if [ "$RESULT" = "255:{10000,zero}" ]
then
	echo "PASS"
else
	echo "FAIL: expected 255:{10000.zero} got $RESULT"
fi
