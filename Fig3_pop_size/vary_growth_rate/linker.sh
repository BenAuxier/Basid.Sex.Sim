for i in 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50
do cat params.dat | sed "s/growth/$i/g" > Temporary_Input.dat && \
./BMS_V5.2_O5 | tee my_fn_growth_$i\_output.txt && rm Bas*.dat
done
