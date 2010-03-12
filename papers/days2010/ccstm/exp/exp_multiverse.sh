# ccstm experiments

high_cont_mult=1
low_cont_mult=64

for pass in 0 1 2; do 
  mkdir -p pass$pass
  for t in 1 2 4 8 16 32 64 128 256; do 
    for f in alpha; do 
      for cont in low high; do 
        echo pass-$pass-$t-$f-$cont
        if [ $cont = high ]; then
          a=`expr $t \* $high_cont_mult`
        elif [ $t = 1 ]; then
          a=2
        else
          a=`expr $t \* $low_cont_mult`
        fi
        out=pass$pass/multiverse-cont_$cont-$t.out
        if grep 'Nb iterations' $out >/dev/null 2>&1; then
          echo "$out already present and valid, skipping"
        else
          ../../../../bin/proj_java --multiverse org.deuce.benchmark.Driver -n $t -w 4000 org.deuce.benchmark.bank_mult.Benchmark -n $a -i 100000  > $out
        fi
      done
    done
  done
done
