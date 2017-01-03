function llike=f2_resar(parm,y,wy,x,meany,meanwy,meanx,N,T,K,detval)
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
rho=parm(1);
beta=parm(1:K+1);
teta=parm(K+2);
sige=parm(K+3);
%
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
%
ee=ones(T,1);
eteta=ee-teta;
yran=y-kron(eteta,meany);
wyran=wy-kron(eteta,meanwy);
xran=x-kron(eteta,meanx);
res=yran-[wyran xran]*beta;
res2=res'*res;
llike=-(N*T/2)*log(2*pi*sige)+T*detm+N/2*log(teta^2)-1/(2*sige)*res2;