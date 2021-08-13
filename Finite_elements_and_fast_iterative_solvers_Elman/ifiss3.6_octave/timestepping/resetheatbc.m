function unew = resetheatbc(uold,xy,bound,t)
%RESETHEATBC resets boundary values in unsteady diffusion 
%   unew = resetheatbc(uold,xy,bound,t);
%   input
%          uold       temperature  vector
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%          t          current time
%   output
%          unew        updated temperature vector  
%
%   calls function specific_bc
%   IFISS function: DJS; 6 December 2009.
% Copyright (c) 2009 D.J. Silvester, D.F. Griffiths, M. Schneider  
nvtx = length(uold); nbd=length(bound);
unew=uold; 
%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
bc=specific_bc(xbd,ybd);
% smoothly grow the hot wall profile
bc=bc*(1-exp(-10*t)); 
unew(bound)=bc; 
return
