# Basid.Sex.Sim
Simulation software for Auxier et al. 2020 manuscript.

This is the source code for the simlation engine. The program, coded in Fortran, uses the procedures_dff.f90 as a dependancy.

To compile use the following commands:

`gfortran -c procedures.f90`

Then enter:

`gfortran -o BMS_mating_counter_V5 BMS_mating_counter.f90 procedures_dff.f90`

The code show now be compiled. To use the program, simulation settings are edited from the Temporary_Input.dat, which is a list of the following parameters:
#Mating Types
Linakge
Dominance

...


Cell size

Once these settings are set as desired, the program is run by entering the command:

`./BMS_mating_counter_V5`

The output of the files is a set of .dat files, which contain the fitness values for each cell, and corresponding mating types. The program also prints the population size and average fitness values of spores/mycelia at each generation. Using modifications of the attached bash scripts these outputs can be parsed for analysis.
