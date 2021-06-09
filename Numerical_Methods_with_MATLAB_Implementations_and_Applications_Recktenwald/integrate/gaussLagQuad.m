function I = gaussLagQuad(fun,nnode,wtype,varargin)
% gaussLagQuad  Gauss-Laguerre quadrature for integrals on [0,infinity)
%
% Synopsis:  I = gaussLagQuad(fun,node)
%            I = gaussLagQuad(fun,node,wtype)
%            I = gaussLagQuad(fun,node,wtype,arg1,arg2,...)
%
% Input:     fun   = (string) name of m-file that evaluates f(x)
%            nnode = number of nodes on each subinterval
%            wtype = (optional) flag indicating how weight function is applied
%                    If wtype = 1 (default) the integrand is of the form
%                    exp(-x)*f(x) and the quadrature rule is sum(w(i)*f(x(i))).
%                    If wtype = 2 the integrand is of the form f(x)
%                    and the quadrature rule is sum(w(i)*exp(x(i))*f(x(i))).
%            arg1,arg2,... = (optional) parameters passed through to fun
%
% Output:    I = approximate value of the integral

if nargin<3,  wtype = 1;  end

if nnode<=25
  [x,w] = GLagTable(nnode);         %  Look up nodes and weights,
else                                %  or if n is too big for the table,
  [x,w] = GLagNodeWt(nnode);        %  compute the nodes and weights
end

f = feval(fun,x,varargin{:});
if wtype == 1
  I = w'*f;             %  int(exp(-x)*dx) = sum(w*f)
else
  we = w.*exp(x);       %  Use rule in the form int(f*dx) = sum(w*exp(x)*f)
  I = we'*f;            %  not int(exp(-x)*dx) = sum(w*f)
end
