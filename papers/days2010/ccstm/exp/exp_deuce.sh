# ccstm experiments

high_cont_mult=1
low_cont_mult=64

for pass in 0 1 2; do 
    for t in 1 2 4 8 16 32 64 128 256; do 
	for f in tl2 lsa; do 
	    for cont in low high; do 
		echo pass-$pass-$t-$f-$cont
		if [ $t -eq 1 ] 
		then 
		    acct=2; 
		else 
		    if [ $cont = high ]
		    then
			acct=`expr $t \* $high_cont_mult`
		    else
		        acct=`expr $t \* $low_cont_mult`
		    fi
		fi; 
		../../../../bin/proj_java --deuce --$f org.deuce.benchmark.Driver -n $t -w 4000 org.deuce.benchmark.bank.Benchmark -n $acct  -i 1000000  > pass$pass/deuce-cont_$cont-$f-$t.out
	    done; 
	done; 
    done; 
done