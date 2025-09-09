#!/bin/bash

N=$1

if [ -z "$N" ]; then

        #If variable is empty
        echo "Script requires the first argument is an integer greater than 0"
        exit 1

else

    if [[ $N =~ ^-?[0-9]+$ ]]; then #is N an integer

        if [ $N -le 0 ]; then

             echo "First Argument must be an integer greater than 0"
             exit 3
        fi
    else

        #N is not an integer
        echo "First Argument must be an integer greater than 0"
        exit 2
    fi 
fi

for T in 300 600 800; do #Create directory for 3300, 3600, 3800
   mkdir -p ${T}K 
   
   touch ./${T}K/water_prop_$N.csv #create csv files to store the data 
   echo "pressure [MPa], density [g/ml], viscosity [muPa*s], enthalpy [kJ/kg]" > ./${T}K/water_prop_$N.csv   
   for i in  $(seq 0 $(($N - 1)) ); do #Loop of pressure 

       P_i=$(echo  "scale = 3; 100/$N*($i+0.5)" | bc -l) #calculate pressure
       d=$(./thermo_water $T ${P_i} density) #calculate density
       v=$(./thermo_water $T ${P_i} viscosity) #calculate viscosity
       e=$(./thermo_water $T ${P_i} enthalpy) #calculate enthalpy
       echo "${P_i}, $d, $v, $e" >> ./${T}K/water_prop_$N.csv #append to csv
   done
done



