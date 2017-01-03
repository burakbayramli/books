function x=selif(y,cond);
% PURPOSE: select values of x for which cond is true
% -----------------------------------------------------
% USAGE: x = selif(y,cond)
%  where    y = input vector 
%        cond = a vector of 0,1 values  
% -----------------------------------------------------
% RETURNS: x = y(cond == 1)
% ----------------------------------------------------- 
% NOTE: a Gauss compatability function
% -----------------------------------------------------
% SEE ALSO: delif, indexcat

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
       
% Select values of x for which cond is true
x=y(cond==1);
return;
