%truncexp.m
U= rand(1,10^5); Z = -log(1 + U *(exp(-2) - 1)); hist(Z,100)
