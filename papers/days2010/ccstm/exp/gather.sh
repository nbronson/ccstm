#!/bin/bash

for e in cont_low cont_high; do
  for t in threads 1 2 4 8 16 32 64 128 256; do
    /bin/echo -n "$t,"
    for c in ccstm-$e-{d,rfw,t} deuce-$e-{lsa,tl2} multiverse-$e; do
      if [ $t = threads ]; then
        /bin/echo -n "$c,"
        continue
      fi
      grep 'TOTAL: T' pass*/$c-$t.out 2>/dev/null | \
          sed -e 's/.*T=//' -e 's/,.*//' | \
          awk '{x += $1} END {printf("%d,", (NR==0 ? 0 : x/NR))}'
    done
    /bin/echo ""
  done
  /bin/echo ""
done
