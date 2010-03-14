#!/bin/bash

ECHO=/usr/ucb/echo
AWK=gawk
if [ ! -x $ECHO ]; then
  ECHO=/bin/echo
  AWK=awk
fi

for m in tflop niagara3; do
  $ECHO "$m -------------------"
  for e in cont_low cont_high; do
    for t in threads 1 2 4 8 16 32 64 128 256; do
      $ECHO -n "$t,"
      for c in ccstm-$e-{d,rfw,t} deuce-$e-{lsa,lsacm,tl2,tl2cm} multiverse-$e; do
        if [ $t = threads ]; then
          $ECHO -n "$c,"
          continue
        fi
        egrep '(TOTAL: T|Nb iterations)' $m/pass*/$c-$t.out 2>/dev/null | \
            sed -e 's/^[^=]*= *//' -e 's/,.*//' | \
            $AWK '{x += $1} END {printf("%d,", (NR==0 ? 0 : x/NR))}'
      done
      $ECHO ""
    done
    $ECHO ""
  done
  $ECHO ""
  $ECHO ""
done
