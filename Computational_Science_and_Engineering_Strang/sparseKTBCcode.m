%1.1  sparseKTBCcode.m

%THIS IS THE CODE TO CREATE K,T,B,C AS SPARSE MATRICES
%Then Matlab will not operate on all the zeros! 

function K=fdmodel(type,n,sparse)
%FDMODEL Create finite difference model matrix.
%   K=FDMODEL(TYPE,N,SPARSE) creates model matrix TYPE
%   of size N-by-N. TYPE is one of the characters 'K', 'T'
%   'B', or 'C'. The matrix is dense unless SPARSE is true.
%
%   K=FDMODEL uses the defaults TYPE='K', N=10, and
%   SPARSE=false.

if nargin<1, type='K'; end
if nargin<2, n=10; end

e=ones(n,1);
K=spdiags([-e,2*e,-e],-1:1,n,n);
switch type
 case 'K'
 case 'T'
  K(1,1)=1;
 case 'B'
  K(1,1)=1;
  K(n,n)=1;
 case 'C'
  K(1,n)=-1;
  K(n,1)=-1;
 otherwise  
  error('Unknown matrix type.');
end

if nargin<3 | ~sparse
  K=full(K);
end
