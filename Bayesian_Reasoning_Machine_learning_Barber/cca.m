function [A,B]=cca(X,Y,M)
%CCA canonical correlation analysis
% [A,B]=cca(X,Y,M)
dimx=size(X,1); dimy=size(Y,1);
if M>min(dimx,dimy); error('M is too large'); end
Sall = cov(vertcat(X,Y)'); 
Sall = Sall+0.00001*eye(size(Sall,1)); % add small amount of noise to encourage invertability
Sxx=Sall(1:dimx,1:dimx);
Syy=Sall(dimx+1:end,dimx+1:end);
Sxy=Sall(1:dimx,dimx+1:end);
iSxx=inv(sqrtm(Sxx));
iSyy=inv(sqrtm(Syy));
[U S V]=svd(iSxx*Sxy*iSyy);
A=iSxx*U(:,1:M);
B=iSyy*V(:,1:M);