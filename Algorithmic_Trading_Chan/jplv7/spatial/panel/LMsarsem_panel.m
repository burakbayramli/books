function LMsarsem_panel(results,W,y,x)
% PURPOSE: Computes (robust) LM tests for spatial lag and spatial error
% model of a panel data model
% -------------------------------------------------------------------------
% Usage: LM=LMsarsem_panel(results,W)
% where: results = a structure returned by a spatial panel regression
%              W = spatial weights matrix (standardized)
%              y = dependent variable vector
%              x = independent variables matrix
% -------------------------------------------------------------------------
% RETURNS: print of lm tests and probabilities
% -------------------------------------------------------------------------
% Note: probabilitities smaller than 0.05 point to signifance of spatial
%       lag or spatial error
% -------------------------------------------------------------------------
% Written by: J.Paul Elhorst summer 2008
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCE: 
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.
%
tr=trace((W'+W)*W);
[N junk]=size(W);
[nobs junk]=size(x);
T=nobs/N;
beta=results.beta;
res=results.resid;
sige=res'*res/nobs;
WXB2=0;EWE=0;EWY=0;
for t=1:T
    t1=(t-1)*N+1;t2=t*N;
    WXB=W*x(t1:t2,:)*beta;
    M=eye(N)-x(t1:t2,:)*inv(x(t1:t2,:)'*x(t1:t2,:))*x(t1:t2,:)';
    WXB2=WXB2+WXB'*M*WXB;
    EWE=EWE+res(t1:t2,1)'*W*res(t1:t2,1);
    EWY=EWY+res(t1:t2,1)'*W*y(t1:t2,1);
end
Ttr=T*tr;
J=(WXB2+Ttr*sige)/sige;
LMerror=(EWE/sige)^2/Ttr;
LMlag=(EWY/sige)^2/J;
robustLMerror=((EWE-(Ttr/J)*EWY)/sige)^2/(Ttr*(1-Ttr/J));
robustLMlag=((EWY-EWE)/sige)^2/(J-Ttr);
fprintf(1,'LM test no spatial lag, probability          = %9.4f,%9.3f \n',LMlag,1-chis_prb(LMlag,1));
fprintf(1,'robust LM test no spatial lag, probability   = %9.4f,%9.3f \n',robustLMlag,1-chis_prb(robustLMlag,1));
fprintf(1,'LM test no spatial error, probability        = %9.4f,%9.3f \n',LMerror,1-chis_prb(LMerror,1));
fprintf(1,'robust LM test no spatial error, probability = %9.4f,%9.3f \n',robustLMerror,1-chis_prb(robustLMerror,1));
