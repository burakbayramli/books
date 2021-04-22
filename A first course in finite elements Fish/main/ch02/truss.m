%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2D Truss (Chapter 2)        %
% Haim Waisman, Rensselaer    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all;
close all; 
 
% include global variables
include_flags;  

% Preprocessor Phase 
 [K,f,d]	= preprocessor;

% Calculation and assembly of element matrices
for e = 1:nel
    ke	= trusselem(e);
    K	= assembly(K,e,ke);
end

% Solution Phase
 [d,f_E]	= solvedr(K,f,d);
 
% Postprocessor Phase 
postprocessor(d)