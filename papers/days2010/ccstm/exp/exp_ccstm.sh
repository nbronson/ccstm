# ccstm experiments

high_cont_mult=1
low_cont_mult=8

for pass in 0 1 2; do 
    for t in 1 2 4 8 16 32 64; do 
	for f in d rfw t; do 
	    for cont in low high; do 
		echo pass-$pass-$t;
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
		echo "../../../../bin/proj_scala edu.stanford.ppl.stm.bank.Benchmark -n $t -a $acct -s 4000 -i 1000000 -f $f > pass$pass/ccstm-cont_$cont-$f-$t.out";  
	    done; 
	done; 
    done; 
done