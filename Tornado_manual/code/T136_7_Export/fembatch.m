%%
clear
load('aircraft\test_geo_1.mat');
load('state\test_state_1.mat');

latticetype=1;
results.void=0;
JID='test';

[lattice,ref]=fLattice_setup2(geo,state,latticetype);    %Setting up the lattice
solverloop5(results,25,JID,lattice,state,geo,ref);% Aeroelast




return