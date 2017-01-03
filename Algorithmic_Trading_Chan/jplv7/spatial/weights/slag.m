function WW = slag(W,p)
% PURPOSE: compute spatial lags
% ---------------------------------------------
% USAGE: Wp = slag(W,p)
% where: W = input spatial weight matrix, sparse or full 
%            (0,1 or standardized form)
%        p = lag order (an integer)
% ---------------------------------------------
% RETURNS: Wp = W^p spatial lag matrix 
%               in standardized form with
%               row-sums of unity
% ---------------------------------------------
% REFERENCES: Anselin and Smirnov 'Efficient Algorithms
% for Constructing Proper Higher Order spatial lag operators',
% West Virginia University Research Paper 9432
% ---------------------------------------------


% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% based on hicon Gauss algorithm of Anselin

if nargin == 2
[n n2] = size(W);
 if n ~= n2
 error('slag: input matrix not square');
 end;
else
 error('slag: wrong # of input arguments');
end;
rflag = 0;

if p == 1
WW = W;
return;
end;

if ~issparse(W),
  W = (W > 0)*1.0; % if matrix is standardized, unstandardize it
  W = W*1.0;
 A = W;
 WP = W;
 WW = zeros(n,n);
 for i = 1:p-1
  WP = WP*W;
  B = (WP > 0);
  B = diagrv(B,zeros(n,1));
  E = B - A;
  WW = (E > 0);
  A = A + WW;
 end;
  WW = normw(WW);
 
else, % we have a sparse matrix input 
 W = (W > 0)*1.0; % unstandardize it
 W = W*1.0;
 WP = W;
 A  = W;
 for ii=1:p-1
 WP = sparse(WP)*sparse(W);
 [i1,j1,s1] = find(WP);
 d = find(i1 == j1); %  replace diagonals with 0's
 s1(d,1) = 0;
 B = sparse(i1,j1,s1,n,n);
 E = sparse(B) - sparse(A);
 [i2,j2,s2] = find(E>0);
 WW = sparse(i2,j2,s2,n,n);
 A = A + sparse(WW);
 end;
 clear A;
 clear WP;
 clear B;
 clear E;
 clear i1 j1 s1 i2 j2 s2;
 WW = normw(WW);
end;
