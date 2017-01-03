function index = find_big(x,number)
% PURPOSE: finds rows where at least one element is > #
%          and returns an index to these rows.
% ---------------------------------------------------------
% USAGE: index = find_big(x,number)
% where:      x = a matrix
%        number = the criterion
% ---------------------------------------------------------
% RETURNS: index = a vector of indices to the
% rows in the matrix x that contain a least one
% element greater than number.
% For example: if x = [0 10 1 0 1
%                      1 2  3 10 10
%                      1 2 3 4 5]
%              and number = 9
%              index = [1
%                       2]                                                                              
% ---------------------------------------------------------
% SEE ALSO: find_nzip()
% ---------------------------------------------------------

 
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs nvar] = size(x);

chk = [];
for i=1:nvar;
tst = find(x(:,i) > number);
chk = [chk 
       tst];
end;

index = unique(chk);