#This script is there to remove the average from every component
#obtained with the dippol code
#The correlation functions that I wanted to study where
#f(t) = < delta a(t) . delta m(0) >
#where
#delta X = X - <X>

#Compilation
> gfortran -O2 delta.d90 -o delta

#Use with one file
> delta dippol_1P_iso.dat

#Use with sevral files
#If you have multiple files, the average will be done over all
#the files
> delta dippol_1P_iso.dat dippol2.dat dippol3.dat
