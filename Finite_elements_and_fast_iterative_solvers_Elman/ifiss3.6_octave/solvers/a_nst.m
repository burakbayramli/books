function y = a_nst(x,params)
%A_NST matrix-vector product for saddle-point operator
%   y = a_nst(x,params)
%   input
%          x            operand for matrix-vector product
%          params       structure defining coefficient matrix
%   output
%          y            result of matrix-vector product
%
%   IFISS function: HCE; 1 April 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage
nu = length(params.F);
np = size(params.B,1);

y = [params.F * x(1:nu) + params.B' * x(nu+1:nu+np); ...
     params.B * x(1:nu) + params.D  * x(nu+1:nu+np)];    
