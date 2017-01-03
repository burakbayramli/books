function llike = f2_sar2_panel(parm,index,y,y1,y2,x,W,N,T)
% written by: J.Paul Elhorst 2/2007
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst J.P., Fréret S. (2009) Evidence of political yardstick competition in France 
% using a two-regime spatial Durbin model with fixed effects. 
% Journal of Regional Science. Forthcoming.

k = length(parm);
b = parm(1:k-3,1);
rho1 = parm(k-2,1);
rho2 = parm(k-1,1);
sige = parm(k,1);
detm=zeros(T,1);
for t=1:T
    W1=W;
    W2=W;
    for i=1:N
        if (index((t-1)*N+i,1)==0) W2(i,1:N)=zeros(1,N);
        else W1(i,1:N)=zeros(1,N);
        end
    end
    IW=eye(N)-rho1*W1-rho2*W2;
    lambda=eig(IW);
    detm(t)=sum(log(lambda));
end
e = y-x*b-rho1*y1-rho2*y2;
epe = e'*e;
tmp2 = 1/(2*sige);
llike = -(N*T/2)*log(2*pi) - (N*T/2)*log(sige) + sum(detm) - tmp2*epe;