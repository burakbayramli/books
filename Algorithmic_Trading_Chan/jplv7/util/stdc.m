function m=stdc(x);
% PURPOSE: standard deviation of each column
% -----------------------------------------------------
% USAGE: x = stdc(y)
%  where    y = input vector 
% -----------------------------------------------------
% RETURNS:  x = standard deviations
% ----------------------------------------------------- 
% NOTE: a Gauss compatability function
% -----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
       
% get standard deviation of each column
m=std(x);
if size(m,2)>1;
   m=m';
end;
