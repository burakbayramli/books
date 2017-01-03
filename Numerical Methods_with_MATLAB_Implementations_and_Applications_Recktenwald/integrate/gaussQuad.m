function I = gaussQuad(fun,a,b,npanel,nnode,varargin)
% gaussQuad  Composite Gauss-Legendre quadrature
%
% Synopsis:  I = gaussQuad(fun,a,b,npanel)
%            I = gaussQuad(fun,a,b,npanel,nnode)
%            I = gaussQuad(fun,a,b,npanel,nnode,arg1,arg2,...)
%
% Input:  fun    = (string) name of m-file that evaluates f(x)
%         a,b    = lower and upper limits of the integral
%         npanel = number of panels in interval [a,b]
%         nnode  = (optional) number of nodes on each subinterval
%                  Default:  nnodes=4
%         arg1,arg2,... = (optional) parameters passed through to fun
%
% Output: I = approximate value of the integral from a to b of f(x)*dx

if nargin<5,  nnode = 4;  end

if nnode<=8
  [z,wt] = GLTable(nnode);           %  Look up nodes and weights,
else                                 %  or if n is too big for the table,
  [z,wt] = GLNodeWt(nnode);          %  compute the nodes and weights
end
H = (b-a)/npanel;                    %  Size of each panel
H2 = H/2;                            %  Avoids repeated computation of H/2
x = a:H:b;                           %  Divide the interval
I = 0;                               %  Initialize sum
for i=1:npanel
  xstar = 0.5*(x(i)+x(i+1)) + H2*z;  %  Evaluate 'fun' at these points
  f = feval(fun,xstar,varargin{:});
  I = I + sum(wt.*f);                %  Add contribution of this subinterval
end
I = I*H2;                            %  Factor of H/2 for each subinterval
