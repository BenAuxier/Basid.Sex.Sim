for i in 0.5 0.6667 0.8 1 1.2 1.5 2
do cat params.dat | sed "s/beta/$i/g" > Temporary_Input.dat && \
./BMS_V5.3 | tee my_fn_beta_$i\_output.txt && rm Bas*.dat
done
