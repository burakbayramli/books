function x_it = m_masscheb(x_it,qparams)
%M_MASSCHEB mass matrix Chebyshev preconditioning operator
%   x_it = m_masscheb(x_it,qparams);
%   input
%          x_it       operand for preconditioning operator
%       qparams       structure for preconditioner:
%            .Q       mass matrix
%          .its       number of iterations of Cheyshev semi-iteration
%      .qmethod       order of approximation
%   output
%          x_it       result of preconditioning operation
%
%   IFISS function: DJS; 2 January 2011.
% Copyright (c) 2010 D.J. Silvester, Tyrone Rees

%% unpack qparams structure
M=qparams.Q; its=qparams.its; qmethod=qparams.qmethod;
np=length(M(1,:));
y0 = zeros(length(x_it),1); b=x_it;
y00 = y0;
w = 1;
        if qmethod == 1 | qmethod == -2
            lmin = 1/4;       % Min eig
            lmax = 9/4;       % Max eig
        elseif qmethod == 2
            lmin = 1/4;        
            lmax = 25/16;
        elseif qmethod == -1
            lmin = 1/4;        
            lmax = 9/4;
        end
alp = (lmin+lmax)/2;           %1/Relaxation parameter           
rho = (lmax-lmin)/(lmax+lmin); 
Mdiag=alp*diag(M); 
%
% iteration loop
for k = 1:its
   w = 1/(1-(w*rho^2)/4);
   r = b - M*y0;
   z = Mdiag.\r; 
   y = w*(z + y0 - y00) + y00;
   y00 = y0; y0 = y;
end
x_it=y;
