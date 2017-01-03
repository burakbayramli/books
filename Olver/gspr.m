function gspr(c,mode,T,dt)
%
%  gspr(c,mode,T,dt)
%
%  Graph the normal dynamical modes of oscillation
%    of a linear one-dimensional mass-spring system
%
%  c = vector indicating which combination of modes to be plotted
%
% Optional arguments:
%
%  mode = 'f'  -- both ends fixed 
%         'd'  -- one free and one fixed end  (default if omitted)
%         'u'  -- both ends free
%  T  -- final time
%  dt -- time step size
%
%   See also DSPR, CSPR, MSPR

if nargin < 4, dt = .5; end
if nargin < 3, T = 100; end
if nargin < 2, mode = 'd'; end

n = size(c,2);  v = c;

clf;

A = diag(ones(n+1,1)) - diag(ones(n,1),-1);
A(:,n+1) = [];

switch mode
	case 'd'
		A(1,:) = []; 
	case 'u'
		A(n+1,:) = []; A(1,:) = []; 
end

K = A' * A;

[E,D] = eig(K);
d = diag(D);
[d,I] = sort(d);
D = diag(d);
E = E(:,I);
l = sqrt(d);

tol = .00001;
zerol = abs(l) < tol;

r = .5;

w = E * v';

s = [0:dt:T];
m = size(s,2);

x = zeros(n,m);

if size(v) == [1,1]
	x = E(:,v) * r * (sin(l(v)*s) + zerol(v)*s);
else
	for i=1:n
		x = x + v(i) * E(:,i) * (sin(l(i)*s) + zerol(i)*s);
	end
end

mx = max(max(abs(x))) + .3;

for i=1:n
  subplot(n,1,i); plot(s,x(i,:)); axis([0 T -mx mx]); title([' mass ',num2str(i)])
end


