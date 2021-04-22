%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Heat conduction in 2D (Chapter 8)           %
% Haim Waisman, Rensselaer                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all;
close all; 

% include global variables
include_flags;

% Preprocessing
[K,f,d] = preprocessor;

% Evaluate element conductance matrix, nodal source vector and assemble
for e = 1:nel
    [ke, fe] = heat2Delem(e); 
    [K,f] = assembly(K,f,e,ke,fe);
end

% Compute and assemble nodal boundary flux vector and point sources
f = src_and_flux(f);

% Solution
[d,f_E] = solvedr(K,f,d);

% Postprocessor
postprocessor(d);
