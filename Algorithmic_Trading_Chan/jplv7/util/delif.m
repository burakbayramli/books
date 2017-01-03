function x=delif(y,cond)
% PURPOSE: select values of x for which cond is false
% -----------------------------------------------------
% USAGE: x = delif(y,cond)
%  where    y = input vector 
%        cond = a vector of 0,1 values  
% -----------------------------------------------------
% RETURNS: x = y(cond == 0)
% ----------------------------------------------------- 
% NOTE: a Gauss compatability function
% -----------------------------------------------------
% SEE ALSO: selif, indexcat

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
       
% Select values of x for which cond is false
x=y(cond==0);
