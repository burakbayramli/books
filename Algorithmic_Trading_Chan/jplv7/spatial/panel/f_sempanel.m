function lik = f_sempanel(rho,eD,W,detval,T)
% PURPOSE: evaluates concentrated log-likelihood for the 
%  spatial panel error model using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:llike = f_sem(rho,eD,W,detm,T)
%  where: rho  = spatial error parameter
%         eD   = begls residuals
%         W    = spatial weight matrix
%       detval = matrix with [rho log determinant] values
%                computed in sem_panel.m using one of 
%                Kelley Pace's routines
%         T    = number of time points
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value at the parameter rho
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% partly rewritten and updated by J.P. Elhorst summer 2008 to account for spatial panels
% REFERENCES: 
% Elhorst JP (2003) Specification and Estimation of Spatial Panel Data Models,
% International Regional Science Review 26: 244-268.
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.

N = length(W); 
gsize = detval(2,1) - detval(1,1);
i1 = find(detval(:,1) <= rho + gsize);
i2 = find(detval(:,1) <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
detm = detval(index,2);
B = speye(N) - rho*W;
Be=zeros(N*T,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    Be(t1:t2,1)= B*eD(t1:t2,1);
end
epe = Be'*Be;
lik = (N*T/2)*log(epe) - T*detm;