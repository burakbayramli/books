function out = merge_matrices(key1,key2,matrix1,matrix2)
% PURPOSE: merge matrices using ID keys
% ---------------------------------------------------------
% USAGE: merged = merge_matrices(key1,key2,matrix1,matrix2)
% where: key1 is from matrix1, 
%        key2 is from matrix2,
%        matrix1 = nobs by nvars1
%        matrix2 = nobs by nvars2
% RETURNS:
% merged = a matrix = [matrix1 matrix2]
%          nobs by nvars1 + nvars2
%          where matrix2 has been sorted by key1
% ---------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


if nargin ~= 4
        error('merge_matrices: requires 4 input arguments');
end;

[nobs,nv1] = size(key1);
[nobsc,nv2] = size(key2);
if nobs ~= nobsc
    error('merge_matrices: key1 and key2 vectors must be the same size');
end;
if nv1 ~= 1
      error('merge_matrices: key1 must be an nobs x 1 vector');
end;
 
if nv2 ~= 1
      error('merge_matrices: key2 must be an nobs x 1 vector');
end;

[junk,nvars1] = size(matrix1);
[junk,nvars2] = size(matrix2);


out = zeros(nobs,nvars1+nvars2);
for i=1:nobs;
    keyi = key1(i,1);
    ind = find(key2 == keyi);
    if length(ind) == 1
    out(i,:) = [matrix1(i,:) matrix2(ind,:)];
    elseif length(ind) == 2
        error('merge_matrices: more than 1 item in key2 matches key1');
    elseif length(ind) == 0
        out(i,:) = [matrix1(i,:) zeros(1,nvars2)];
    end;
end;
    
    
