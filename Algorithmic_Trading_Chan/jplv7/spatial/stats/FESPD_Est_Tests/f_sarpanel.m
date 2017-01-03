function llike = f_sarpanel(rho,detval,epe0,eped,epe0d,N,T)
% PURPOSE: evaluates concentrated log-likelihood for the 
%  spatial panel autoregressive model using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:llike = f_sar(rho,detval,epe0,eped,epe0d,n,T)
%  where: rho  = spatial autoregressive parameter
%         detval = a matrix with vectorized log-determinant information
%         epe0   = see below
%         eped   = see below
%         eoe0d  = see below
%         N      = number of spatial units
%         T      = number of time points
%         b0 = AI*xs'*ys;
%         bd = AI*xs'*Wys;
%         e0 = ys - xs*b0;
%         ed = Wys - xs*bd;
%         epe0 = e0'*e0;
%         eped = ed'*ed;
%         epe0d = ed'*e0;
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value at the parameter rho
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

% partly rewritten and updated by J.P. Elhorst summer 2008 to account for spatial panels
% REFERENCES: 
% Elhorst JP (2003) Specification and Estimation of Spatial Panel Data Models,
% International Regional Science Review 26: 244-268.
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.

gsize = detval(2,1) - detval(1,1);
% Note these are actually log detvalues
i1 = find(detval(:,1) <= rho + gsize);
i2 = find(detval(:,1) <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
detm = detval(index,2); 
z = epe0 - 2*rho*epe0d + rho*rho*eped;
llike = (N*T/2)*log(z) - T*detm;