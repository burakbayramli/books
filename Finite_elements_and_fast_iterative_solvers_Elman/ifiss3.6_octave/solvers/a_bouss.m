function y = a_bouss(x,params)
%A_BOUSS matrix-vector product for Boussinesq operator
%   y = a_bouss(x,params)
%   input
%          x            operand for matrix-vector product
%          params       structure defining coefficient matrix
%   output
%          y            result of matrix-vector product
%
%   IFISS function: DJS; 27 April 2012.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic.
np = size(params.B,1);
nu = size(params.B,2);
nt = size(params.Mt,2);

y = [params.F  * x(1:nu) + params.B' * x(nu+1:nu+np) ... 
                 - params.Mt * x(nu+np+1:nu+np+nt); ...
     params.B  * x(1:nu) + params.D  * x(nu+1:nu+np); ...   
     params.Ft * x(nu+np+1:nu+np+nt); ]; 
 return
