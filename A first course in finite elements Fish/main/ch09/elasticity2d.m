%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2D Elasticity  (chapter 9)   %
% Haim Waisman, Rensselaer     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all;
close all; 

% include global variables
include_flags;

% Preprocessing 
[K,f,d] = preprocessor;

% Assembly
for e = 1:nel
    [ke, fe] = elast2Delem(e); 
    [K,f] = assembly(K,f,e,ke,fe);
end

% Compute and assemble nodal boundary force vector and point forces
f = point_and_trac(f);

% Solution Phase
[d,f_E] = solvedr(K,f,d);

% Postprocessor
postprocessor(d);
