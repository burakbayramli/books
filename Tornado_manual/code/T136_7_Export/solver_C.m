function [results]=solver_C(results,state,geo,lattice,ref)
%This function calls the Eqvivalen C functions to solve.

fExportLattice( lattice ) ;  %Generate a textfile "lattice.txt"
!downwash.exe                %Computes the downwash matrix and save it in "downwash.txt"




end

