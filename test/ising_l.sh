#! /bin/bash

bins=3
mc_eq=10000
mcs=10000

x_i=0.3
x_f=3
x_step=0.01

for ll in 8
do
	temp=$x_i
	while(( $(echo "$temp <= $x_f" | bc ) ))
	
	do
	echo "$ll $temp $mc_eq $bins $mcs" > read.in
	`gfortran -o ${ll}.out h_model.f90`
	./${ll}.out     # & 
	mv out.dat out_$temp.dat
	awk -f cal_bins.awk out_$temp.dat >> data  #change bins in awk
	rm out_${temp}.dat                         #raw data
	
	temp=$(echo "$temp + $x_step" | bc)
	echo -e "$temp " " \c"
	
	done
	rm ${ll}.out
	`mkdir ${ll}_${x_i}_${x_f}`;`mv data ${ll}_${x_i}_${x_f}`;
	cp cal_kai_c_q.awk ${ll}_${x_i}_${x_f}   #change l

# draw
cd ${ll}_${x_i}_${x_f}
 # awk -f cal_kai_c_q.awk data > quantity
echo "set terminal postscript eps size 10,5 color linewidth 3 font 'Times,20' #这里10，4是图的比例，一些线宽，和统一的字体以及颜色" > draw
echo "set output \"${ll}_${x_i}_${x_f}.eps\"" >> draw
echo "plot \"data\" using 1:2 with lines linetype -1 lw 2 dt 1 title 'L=$ll'" >>draw
gnuplot draw

cd ..
done

cd ..




