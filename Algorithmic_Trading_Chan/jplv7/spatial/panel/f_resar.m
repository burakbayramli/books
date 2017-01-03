function llike=f_resar(teta,beta,y,wy,x,meany,meanwy,meanx,N,T)
% computes teta
%
% written by: J.Paul Elhorst summer 2008
% University of Groningen
% Department of Economics and Econometrics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst JP (2003) Specification and Estimation of Spatial Panel Data Models,
% International Regional Science Review 26: 244-268.
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.
%
ee=ones(T,1);
eteta=ee-teta;
yran=y-kron(eteta,meany);
wyran=wy-kron(eteta,meanwy);
xran=x-kron(eteta,meanx);
res=yran-[wyran xran]*beta;
res2=res'*res;
llike=(N*T)/2*log(res2)-N/2*log(teta^2);