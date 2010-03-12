# ccstm experiments

high_cont_mult=1
low_cont_mult=64

for pass in 0 1 2; do 
  mkdir -p pass$pass
  for t in 1 2 4 8 16 32 64 128 256; do 
    for f in d rfw t; do 
      for cont in low high; do 
        echo pass-$pass-$t-$f-$cont
        if [ $cont = high ]; then
          a=`expr $t \* $high_cont_mult`
        elif [ $t = 1 ]; then
          a=2
        else
          a=`expr $t \* $low_cont_mult`
        fi
        out=pass$pass/ccstm-cont_$cont-$f-$t.out
        if grep '^TOTAL: T' $out >/dev/null 2>&1; then
          echo "$out already present and valid, skipping"
        else
          ../../../../bin/proj_scala edu.stanford.ppl.stm.bank.Benchmark -n $t -a $a -s 4000 -i 1000000 -f $f > $out
        fi
      done
    done
  done
done
