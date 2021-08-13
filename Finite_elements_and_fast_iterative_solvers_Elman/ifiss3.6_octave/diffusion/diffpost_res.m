function [err_res,elerr_res] = diffpost_res(jmp,els,rhsq,hlsq)
%DIFFPOST_RES computes Q1 element residual error estimator 
%   [err_res,elerr_res] = diffpost_res(jmp,els,rhsq,hlsq);
%   input
%          jmp          elementwise edge flux jumps
%          els          elementwise edge lengths
%          rhsq         elementwise L2 residual norms
%          hlsq         elementwise areas
%   output
%          err_res      global residual error
%          elerr_res    elementwise residual errors
%
%   IFISS function: DJS; 1 April 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
elerr_res=0.5*sum((els.*els.*jmp.*jmp)')' + hlsq.*rhsq;
err_res=sqrt(sum(elerr_res));
elerr_res=sqrt(elerr_res);
fprintf('computing residual error estimator... ')
fprintf('\nestimated global error (in energy):  %10.6e\n',err_res)   
return
