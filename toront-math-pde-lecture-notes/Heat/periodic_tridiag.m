% x = periodic_tridiag(A,b) takes a nearly tri-diagonal n x n matrix A
% that has nonzero entries at A(1,n) and A(n,1) and a vector b and
% returns the solution of Ax=b.

function x = periodic_tridiag(A,b)

% The idea here is to write the matrix problem as follows:
%
% a11 v1 a1n   | x1       b1 
%              |
%  v2 T  v3    | xx    =  bb 
%              |
% an1 v4 ann   | xn       bn
%
% where v1,v2,v3,v4,xx,and bb are vectors of length n-2.  and T is a
% tridiagonal matrix.
%
% The problem then becomes
%
% a11 x1 + <v1,xx> + a1n xn = b1
% x1 v2 + T xx + xn v3 = bb
% an1 x1 + <v4,xx> + ann xn = bn
%
% We solve the second equation for xx:  
% T xx = bb - x1 v2 - xn v3
% xx = inv(T)*bb - x1 inv(T)*v2 - xn inv(T)*v3
%
% plug xx into the first and third equations, resulting in a 2x2 problem
% for x1 and xn
% 
% a11 x1 + <v1,inv(T)*bb> -x1 <v1,inv(T)*v2> - xn <v1,inv(T)*v3> + a1n xn = b1
% an1 x1 + <v4,inv(T)*bb> -x1 <v4,inv(T)*v2> - xn <v4,inv(T)*v3> + ann xn = bn
%
% AA(1,1) = a11 - <v1,inv(T)*v2>
% AA(1,2) = a1n - <v1,inv(T)*v3>
% AA(2,1) = an1 - <v4,inv(T)*v2>
% AA(2,2) = ann - <v4,inv(T)*v3>
%
% rhs(1) = b1-<v1,inv(T)*bb>
% rhs(2) = bn-<v4,inv(T)*bb>
%
% once we know x1 and xn, we use them to define xx:
% xx = inv(T)*bb - x1 inv(T)*v2 - xn inv(T)*v3
%
% Note: this uses three calls to tridiag, to find inv(T)*bb,inv(T)*v2,inv(T)*v3
% which is 3*O(n).  But this still beats the LU approach which is O(n^2).

n = length(b);
v1 = zeros(n-2,1);
v2 = zeros(n-2,1);
v3 = zeros(n-2,1);
v4 = zeros(n-2,1);
T = zeros(n-2,n-2);
bb = zeros(n-2,1);

v1(1) = A(1,2);
v2(1) = A(2,1);
v3(n-2) = A(n-1,n);
v4(n-2) = A(n,n-1);

for i=1:n-2
  bb(i) = b(i+1);
  for j=1:n-2
    T(i,j) = A(i+1,j+1);
  end
end

v2i = tri_diag(T,v2);
v3i = tri_diag(T,v3);
bbi = tri_diag(T,bb);

% use that the dot product is the transpose of one vector times the
% other.  ' gives the transpose

AA(1,1) = A(1,1) - v1'*v2i;
AA(1,2) = A(1,n) - v1'*v3i;
AA(2,1) = A(n,1) - v4'*v2i;
AA(2,2) = A(n,n) - v4'*v3i;

rhs(1) = b(1) - v1'*bbi;
rhs(2) = b(n) - v4'*bbi;

rhs = rhs';

v = inv(AA)*rhs;
x(1) = v(1);
x(n) = v(2);

xx = bbi - x(1)*v2i - x(n)*v3i;
for i=2:n-1
 x(i) = xx(i-1);
end 

x = x';


