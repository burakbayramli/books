function [x,v]=simplex(c,A,b,v,options)

%E.K.P. Chong, Mar. 23, 1994 
%               SIMPLEX(c,A,b,v);
%               SIMPLEX(c,A,b,v,options);
%
%               x = SIMPLEX(c,A,b,v);
%               x = SIMPLEX(c,A,b,v,options);
%
%               [x,v] = SIMPLEX(c,A,b,v);
%               [x,v] = SIMPLEX(c,A,b,v,options);
%
%SIMPLEX(c,A,b,v) solves the following linear program using the 
%Simplex Method: 
%   min c'x  subject to Ax=b, x>=0,
%where [A b] is in canonical form, and v is the vector of indices of
%basic columns. Specifically, the v(i)-th column of A is the i-th
%standard basis vector.
%The second variant allows a vector of optional parameters to be
%defined:
%OPTIONS(1) controls how much display output is given; set 
%to 1 for a tabular display of results (default is no display: 0).  
%OPTIONS(5) specifies how the pivot element is selected; 
%   0=choose the most negative relative cost coefficient; 
%   1=use Bland's rule.

if nargin ~= 5 
  options = []; 
  if nargin ~= 4
    disp('Wrong number of arguments.');
    return;
  end 
end 
 
%format compact;
format rat;
%format short e;
 
options = foptions(options);
print = options(1); 

n=length(c);
m=length(b);

cB=c(v(:));

r = c'-cB'*A; %row vector of relative cost coefficients

cost = -cB'*b;

tabl=[A b;r cost];

if print,
  disp(' ');
  disp('Initial tableau:');
  disp(tabl);
end %if

r(v(:)) = 0; %just to be sure!
while ones(1,n)*(r' >= -(10^(-10))*ones(n,1)) ~= n
  if options(5) == 0;
    [r_q,q] = min(r);
  else
    %Bland's rule
    q=1;
    while r(q) >= 0;
      q=q+1;
    end
  end %if
  
  min_ratio = inf;
  p=0;
  for i=1:m,
    if tabl(i,q)>0
      if tabl(i,n+1)/tabl(i,q) < min_ratio
        min_ratio = tabl(i,n+1)/tabl(i,q);
        p = i;
      end %if
    end %if
  end %for
  if p == 0
    disp('Problem unbounded');
    break;
  end %if
  
  tabl=pivot(tabl,p,q);

  if print,
    disp('Pivot point:');
    disp([p,q]);
    disp('New tableau:');
    disp(tabl);
  end %if

  v(p) = q;
  r = tabl(m+1,1:n);
end %while

x=zeros(n,1);
x(v(:))=tabl(1:m,n+1);
