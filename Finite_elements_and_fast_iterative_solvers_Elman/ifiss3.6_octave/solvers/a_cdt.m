function y = a_cdt(x,params)
%A_CDT matrix-vector product for scalar operator
%   y = a_cdt(x,params)
%   input
%          x            operand for matrix-vector product
%          params       structure defining coefficient matrix
%   output
%          y            result of matrix-vector product
%
%   IFISS function: HCE; 1 April 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage
y = params.A * x;
