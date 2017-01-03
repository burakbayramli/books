function mit18086_fillin(n)
%MIT18086_FILLIN
%    Sets up a 2d Poisson problem, and solves the resulting
%    system by Gaussian elimination, for three different
%    orderings of the unknowns: lexicographic, red-black,
%    and symamd.

% 03/2007 by Benjamin Seibold
%            http://www-math.mit.edu/~seibold/
% Feel free to modify for teaching and learning.

if nargin<1, n = 5; end
% works only for odd values so far
A = delsq(numgrid('S',n+2));

[L,U] = lu(A);
clf
subplot(2,3,1), spy(A), title('lexicographic ordering: A')
subplot(2,3,4), spy(U), title('after elimination')

p = [1:2:n^2 2:2:n^2];
[L,U] = lu(A(p,p));
subplot(2,3,2), spy(A(p,p)), title('red-black ordering: A')
subplot(2,3,5), spy(U), title('after elimination')

p = symamd(A);
[L,U] = lu(A(p,p));
subplot(2,3,3), spy(A(p,p)), title('symamd ordering: A')
subplot(2,3,6), spy(U), title('after elimination')
