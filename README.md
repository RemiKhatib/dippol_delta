**This script has been written by Rémi Khatib.
Its goal is to remove the average from every component obtained with the [dippol code](https://github.com/RemiKhatib/dippol).**

The correlation functions that I wanted to study were

  _f(t) = < delta a(t) . delta m(0) >_

where

  _delta X = X - < X >_

# Compilation
```gfortran -O2 delta.f90 -o delta```

# How to use it ?
## Single input file
```delta dippol_1P_iso.dat```

## Multiple input files
If you have multiple files, the average will be done over all the files

```delta dippol_1P_iso.dat dippol2.dat dippol3.dat```
