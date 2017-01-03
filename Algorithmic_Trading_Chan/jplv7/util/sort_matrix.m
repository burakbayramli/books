function out = sort_matrix(key1,key2,matrix)
% PURPOSE: sort matrix using ID keys
% ---------------------------------------------------------
% USAGE: sorted = sort_matrix(key1,key2,matrix)
% where: key1 = the results matrix key, nobs x 1
%        key2 is a key for input matrix, nobs x 1
%        matrix = nobs by nvars input matrix
% RETURNS:
% sorted = a matrix arranged according to key1
%                nobs by nvars 
%                where matrix has been sorted by key1
% ---------------------------------------------------------

if nargin ~= 3
        error('sort_matrix: requires 3 input arguments');
end;

[nobs,nv1] = size(key1);
[nobsc,nv2] = size(key2);
if nobs ~= nobsc
    error('sort_matrix: key1 and key2 vectors must be the same size');
end;
if nv1 ~= 1
      error('sort_matrix: key1 must be an nobs x 1 vector');
end;
 
if nv2 ~= 1
      error('sort_matrix: key2 must be an nobs x 1 vector');
end;

[junk,nvars] = size(matrix);


out = zeros(nobs,nvars);
for i=1:nobs;
    keyi = key1(i,1);
    ind = find(key2 == keyi);
    if length(ind) == 1
    out(i,:) = [matrix(i,:)];
    elseif length(ind) == 2
        error('sort_matrix: more than 1 item in key2 matches key1');
    elseif length(ind) == 0
        out(i,:) = [zeros(1,nvars)];
    end;
end;
    
    
