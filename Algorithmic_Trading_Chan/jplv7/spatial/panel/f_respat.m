function llike=f_respat(b,beta,y,x,wy,wx,lambda,meany,meanx,wmeany,wmeanx,vmeany,vmeanx,N,T,nvar)
% computes teta and spatial autocorrelation
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
rho=b(1);
teta=b(2);
ee=ones(T,1);
eigw=zeros(N,1);
meanpy=zeros(N,1);
meanpx=zeros(N,nvar);
for i=1:N
    eigw(i)=(T*teta^2+1/(1-rho*lambda(i))^2)^(-0.5);
    meanpy(i,1)=eigw(i)*vmeany(i,1)-(meany(i,1)-rho*wmeany(i,1));
    meanpx(i,:)=eigw(i)*vmeanx(i,:)-(meanx(i,:)-rho*wmeanx(i,:));
end
yran=y-rho*wy+kron(ee,meanpy);
xran=x-rho*wx+kron(ee,meanpx);
res=yran-xran*beta;
res2=res'*res;
somp1=0;
somp2=0;
for i=1:N
   p1=1+T*teta^2*(1-lambda(i)*rho)^2;
   p2=1-lambda(i)*rho;
   somp1=somp1+log(p1);
   somp2=somp2+log(p2);   
end
llike=(N*T/2)*log(res2)+1/2*somp1-T*somp2;