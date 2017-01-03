function llike = f_sar2_panel(rhos,index,W,e0,ed1,ed2,N,T)
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

rho1=rhos(1);
rho2=rhos(2);
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
z = (e0-rho1*ed1-rho2*ed2)'*(e0-rho1*ed1-rho2*ed2);
llike = (N*T/2)*log(z) - sum(detm);