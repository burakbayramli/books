function [Mstr,ppo,rro,TTo,AstrA,AAstr] = Isentropic_FlowF(M,Y)
%
% Isentropic flow function
% INPUT: M, Y=cp/cv
% OUTPUT:  M*, p/po, rho/rhoo, T/To, A*/A 
% Coded by Daniel T. Valentine ............ Fall 2009
% Step 1:
fac = (Y-1)/2;
ex1 = Y/(Y-1);
M2 = M.^2;
% Step 2: Computation of dimensionless properties in terms 
% of the stagnation state of the gas:
% To/T
ToT = 1 + fac.*M2;
TTo = 1./ToT;
%po/p
pop = ToT.^ex1;
ppo = 1./pop;
% rhoo/rho
ror = pop.^(1/Y);
rro = 1./ror;
%A*/A
fc1 = 2/(Y+1);
fc2 = (Y-1)/(Y+1);
ex2 = (Y+1)/(2*(Y-1));
AstrA = M./((fc1+fc2.*M2).^ex2);
AAstr = (1./M).*(( 1 + M.^2.*(Y-1)./2 )./...
    ( (Y+1)/2 ) ).^( (Y+1)/(2*(Y-1)));
% M*
Mstr2 = ((Y+1).*M2./2)./(1+fac.*M2);
Mstr = sqrt(Mstr2);
