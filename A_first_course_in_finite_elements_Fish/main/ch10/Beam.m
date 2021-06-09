%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Beam (Chapter 10)                    %
% Suleiman M. BaniHani                 %
% Rensselaer Polytechnic Institute     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all;
close all; 
 
% include global variables
include_flags;  

% Preprocessing 
 [K,f,d]	= preprocessor;

% Element matrix computations and assembly
for e = 1:nel
    [ke,fe]	= beamelem(e);
    [K, f]  = assembly(K,f,e,ke,fe);
end
% Add nodal boundary force vector
f = NaturalBC(f);

% Partition and solution
 [d,f_E] = solvedr(K,f,d);
 
% Postprocessing 
postprocessor(d)

