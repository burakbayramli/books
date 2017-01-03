function llike=f2_serialspat(parm,y,wy,x,wx,N,T,w,V)
% -------------------------------------------------------------------------
% written by: J.Paul Elhorst 5/2008
% University of Groningen
% Faculty of Economics and Business
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst J.P. (2008) Serial and spatial autocorrelation. Economics Letters
% http://dx.doi.org/10.1016/j.econlet.2008.03.009
% -------------------------------------------------------------------------
rho=parm(1);
lambda=parm(2);
si2=parm(3);
for i=1:N
    det1(i,1)=1-lambda*w(i);
    det2(i,1)=1-(rho/(1-lambda*w(i)))^2;
    eigw(i,1)=sqrt(det2(i,1));
end
P=V*diag(eigw);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    if (t==1)
        ytr(t1:t2,1)=P*(y(t1:t2)-lambda*wy(t1:t2));
        xtr(t1:t2,:)=P*(x(t1:t2,:)-lambda*wx(t1:t2,:));
    else
        ytr(t1:t2,1)=y(t1:t2)-lambda*wy(t1:t2)-rho*y(t1-N:t2-N);
        xtr(t1:t2,:)=x(t1:t2,:)-lambda*wx(t1:t2,:)-rho*x(t1-N:t2-N,:);
    end
end
b=inv(xtr'*xtr)*xtr'*ytr;
res=ytr-xtr*b;
edt2=res'*res;
si2=edt2/(N*T);
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-0.5*sum(log(det2))-T*sum(log(det1))+tmp2*edt2;