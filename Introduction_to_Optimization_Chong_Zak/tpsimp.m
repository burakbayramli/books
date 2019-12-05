function [x,v]=tpsimplex(c,A,b,options)
%E.K.P. Chong, Mar. 23, 1994
%
%               TPSIMPLEX(c,A,b);
%               TPSIMPLEX(c,A,b,options);
%
%               x = TPSIMPLEX(c,A,b);
%               x = TPSIMPLEX(c,A,b,options);
%
%               [x,v] = TPSIMPLEX(c,A,b);
%               [x,v] = TPSIMPLEX(c,A,b,options);
%
%TPSIMPLEX(c,A,b) solves the following linear program using the 
%Two-Phase Simplex Method:
%  min c'x  subject to Ax=b, x>=0.
%The second variant allows a vector of optional parameters to be
%defined:
%OPTIONS(1) controls how much display output is given; set 
%to 1 for a tabular display of results (default is no display: 0). 
%OPTIONS(5) specifies how the pivot element is selected;
%  0=choose the most negative relative cost coefficient;
%  1=use Bland's rule.


if nargin ~= 4
  options = [];
    if nargin ~= 3
    disp('Wrong number of arguments.');
    return;
  end
end

%clc;
format compact;
%format short e;

options = foptions(options);
print = options(1);

n=length(c);
m=length(b);

%Phase I
if print,
  disp(' ');
  disp('Phase I');
  disp('-------');
end

v=n*ones(m,1);
for i=1:m
  v(i)=v(i)+i;
end

[x,v]=simplex([zeros(n,1);ones(m,1)], [A eye(m)], b, v, options);

if all(v<=n),
  %Phase II
  if print
    disp(' ');
    disp('Phase II');
    disp('--------');
    disp('Basic columns:')
    disp(v')
  end 

  %Convert [A b] into canonical augmented matrix
  Binv=inv(A(:,[v]));
  A=Binv*A;
  b=Binv*b;

  [x,v]=simplex(c,A,b,v,options);

  if print
    disp(' ');
    disp('Final solution:');
    disp(x');
  end
else 
  %assumes nondegeneracy
  disp('Terminating: problem has no feasible solution.');
end
