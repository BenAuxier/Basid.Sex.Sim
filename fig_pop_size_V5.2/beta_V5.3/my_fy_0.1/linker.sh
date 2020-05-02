for i in 0.00 0.50 0.75 0.90 0.95 0.99 1.00
do cat params.dat | sed "s/linkage/$i/g" > Temporary_Input.dat && \
./BMS_V5.2_O5 | tee my_fy_$i\_output.txt && rm Bas*.dat
done
