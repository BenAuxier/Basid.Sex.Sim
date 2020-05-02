#for i in 0.001 0.005 0.01 0.015 0.02 0.03 0.04 0.05
for i in 0.06 0.07 0.08 0.09 0.10
do cat params.dat | sed "s/mut/$i/g" > Temporary_Input.dat && \
./BMS_V5.2_O5 | tee my_fn_mutate_$i\_output.txt && rm Bas*.dat
done
