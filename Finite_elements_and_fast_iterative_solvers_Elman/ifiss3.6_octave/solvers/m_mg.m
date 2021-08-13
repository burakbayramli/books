function c = m_mg(x_it,aparams,mparams)
%M_MG GMG preconditioner for scalar problems
%   c = m_mg(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      not used
%          mparams      structure defining preconditioning matrix
%   output
%          c            result of MG preconditioning operation
%
%   IFISS function: DJS, HCE; 9 April 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

c = mg_iter(mparams.mgdata,zeros(size(x_it)),x_it,mparams.smooth_data, ...
            mparams.nc,mparams.npre,mparams.npost,mparams.sweeps);
