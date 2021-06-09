%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1D Bar (Chapter 5)         %
% Haim Waisman, Renssealer   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all;
close all; 

% include global variables
include_flags;  

% Preprocessing
[K,f,d] = preprocessor;

% Assembly 
for e = 1:nel
    [ke,fe] = barelem(e);
    [K, f]  = assembly(K,f,e,ke,fe);
end

% Add boundary force vector
f  = naturalBC(f);

% Solution
[d,f_E] = solvedr(K,f,d);

% Postprocessing
postprocessor(d);


% plot the exact solution of stresses
ExactSolution;





