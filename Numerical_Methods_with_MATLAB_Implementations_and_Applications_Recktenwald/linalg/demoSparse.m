% demoSparse  Script demonstrating some sparse matrix commands
%
clc
echo on

% --- Create a sparse matrix, then assign element values one at at time
S = spalloc(5,5,18);        %  5 x 5 sparse matrix with room for 18 nonzeros
S(1,1) = 2;  S(1,2) = -1;   %  first row
echo off                    %  prevent loop from being listed
for k=2:4
  S(k,k-1) = -1;
  S(k,k)   =  2;
  S(k,k+1) = -1;
end
echo on
S(5,4) = -1;  S(5,5) = 2;   %  last row

% --- Inquire about number of zeros, and max room for non-zero elements
nnz(S)
nzmax(S)

% --- Add a couple more entries
S(1,5) = -1;    S(5,1) = -1;
nnz(S)
nzmax(S)

% --- Make a "spy" plot to display structure
spy(S)
echo off
