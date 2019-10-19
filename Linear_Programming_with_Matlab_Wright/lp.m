function [x, lambda] = lp(c,A,b,vlb,vub,x0,numeq,itermx);
%lp     Linear programming (using CPLEX).
%       x=lp(c,A,b) solves the linear programming problem:
%        
%            min c'x    subject to:   Ax <= b 
%             x
%   
%       [x,lambda]=lp(c,A,b) returns lambda = -u, where u are the Lagrange
%	multipliers. That is, u solves the dual problem
%
%	max -b'u    subject to:  -A'u = c,  u >= 0.
% 	 u
%
%       x=lp(c,A,b,vlb,vub) defines a set of lower and upper
%       bounds on the design variables, x, so that the solution is always in
%       the range vlb <= x <= vub.
%
%       x=lp(c,A,b,vlb,vub,x0,numeq) indicates that the first 
%       numeq constraints defined by A and b are equality constraints.
%
%       x=lp(c,A,b,vlb,vub,x0,numeq,itermx) sets the maximum number of 
%       iterations to itermx
%
%       (x0 is completely ignored, but preserved for API compatibility
%       with the LP function on the UNIX boxes

  if (( nargin < 3) | (nargin > 8) )
    error('Wrong # of input parameters');
  end;

  n=length(c);
  m = size(A,1);

  if nargin==3
    [obj,x,lambda]=cplex(c,A,b,-inf*ones(n,1),inf*ones(n,1),[1:m]);
  elseif nargin==4
    if ( length(vlb) ~= n )
      error('Wrong # of lower bounds.');
    end;
    [obj,x,lambda]=cplex(c,A,b,vlb,inf*ones(n,1),[1:m]);
  elseif (nargin==5 | nargin==6)
    if ( length(vub) ~= n )
      error('Wrong # of upper bounds.');
    end;
    [obj,x,lambda]=cplex(c,A,b,vlb,vub,[1:m]);
  elseif (nargin==7)
    [obj,x,lambda]=cplex(c,A,b,vlb,vub,[numeq+1:m]);
  elseif (nargin==8)
    [obj,x,lambda]=cplex(c,A,b,vlb,vub,[numeq+1:m],[],itermx);
    
  end;
