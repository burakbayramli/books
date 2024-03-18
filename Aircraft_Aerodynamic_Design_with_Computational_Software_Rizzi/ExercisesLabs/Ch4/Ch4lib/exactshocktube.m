function [xe,p5,rho5,u5,...
    V,p3,rho3,u3,...
    C,p2,rho2,u2]= exactshocktube(handles)
% % ala Hirsch
%
subfun = 1
if subfun ~= 1
    close all
end
if subfun==1
    R     = handles.Data.R ;
    gamma = handles.Data.gamma;
    pL    = handles.Data.p01;
    t01   = handles.Data.t01;
    rhoL  = pL/(R*t01);
    pR    = handles.Data.p2;
    rhoR  = pR/(R*t01);
else
    R     = 288.7
    gamma = 7/5
    pL    = 100000
    t01   = 300
    rhoL  = pL/(R*t01)
    pR    = 5000
    rhoR  = pR/(R*t01)
end
uL   = 0;
cL   = sqrt(gamma*pL/rhoL)
% backpressure etc.
uR   = 0;
cR   = sqrt(gamma*pR/rhoR)
% Hirsch eq 16.6.45, typo fixed
itmax = 10
alfa = (gamma+1)/(gamma-1);
plopr = pL/pR;
P = 0.5;
coef = 2/(gamma-1)*sqrt(gamma*(gamma-1)/2)*cL/cR;
ggo  = (gamma-1)/(2*gamma);
plop = plopr^(ggo);
for it = 0:itmax
    f   = (P-1)/sqrt(1+alfa*P)-coef/plop*(plop-P^ggo)-(uL-uR)/cR; % typo in Hirsch
    df  = 1/sqrt(1+alfa*P)+(P-1)*(-1/2)*(1+alfa*P)^(-3/2)*alfa + coef/plop*ggo*P^(ggo-1);
    cor = f/df;
    P   = P-0.999*cor;
    if abs(cor/P) < 1e-6
        break
    end
end
% region 2
u2 = cR*(P-1)/sqrt(1+alfa*P)/sqrt(gamma*(gamma-1)/2)+uR;
% shock speed
C    = cR^2*(P-1)/(gamma*(u2-uR))+uR
rho2 = rhoR*(1+alfa*P)/(alfa+P);
c2   = cR*sqrt(P*(alfa+P)/(1+alfa*P));
p2   = c2^2/gamma*rho2;
T2   = p2/(R*rho2);
% region 3
u3 = u2;
V  = u2; % contact speed
p3 = p2;
c3 = cL-(gamma-1)/2*V;
rho3 = gamma*p3/c3^2;
T3   = p3/(R*rho3);
% region 5
xotmax = (gamma+1)/2*V - cL -(gamma-1)/2*uL;
xotmin = -((gamma-1)/2*uL+cL);
np = 10;
xe = linspace(xotmin,xotmax,np);
u5 = 2/(gamma+1)*(xe+cL+(gamma-1)/2*uL);
c5 = u5-xe;
p5 = pL*(c5/cL).^(2*gamma/(gamma-1)); % typo in Hirsch
rho5 = gamma*p5./c5.^2;
T5   = p5./rho5/R;
if ~subfun
    fprintf('%f %f %f %f \n',xotmin,xotmax,V,C)
    plot([2*xotmin,xotmin],[rhoL,rhoL],'.-k')
    hold on
    plot(xe,rho5,'.-k')
    plot([xotmax,V],[rho3,rho3],'.-k');
    plot([V,C],     [rho2,rho2],'.-k');
    plot([C,2*C],   [rhoR,rhoR],'.-k')
    
    plot([2*xotmin,xotmin],[pL,pL]/pR,'.-r')
    plot(xe,p5/pR,'.-r')
    plot([xotmax,V],[p3,p3]/pR,'.-r');
    plot([V,C],[p2,p2]/pR,'.-r');
    
    plot([C,2*C],[pR,pR]/pR,'.-r')
end



