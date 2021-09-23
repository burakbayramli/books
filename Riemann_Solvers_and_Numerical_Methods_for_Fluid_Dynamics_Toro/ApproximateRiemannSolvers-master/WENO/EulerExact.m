function [x,rho,ux,p,e,t,Mach,entro]=EulerExact(rho1,u1,p1,rho4,u4,p4,tEnd)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Classical Gas Exact Riemann Solver 
% Coded by Manuel Diaz, IAM, NTU 03.09.2011.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Riemann Solver for solving shoc-tube problems
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This programs was modified by Manuel Diaz, and is based on the code of 
% [1]  P. Wesseling. PRINCIPLES OF COMPUTATIONAL FLUID DYNAMICS
% Springer-Verlag, Berlin etc., 2001. ISBN 3-540-67853-0
% See http://dutita0.twi.tudelft.nl/nw/users/wesseling/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE:
% A Cavitation Check is the is incorporated in the code. It further
% prevents plotting for possible but physically unlikely case of expansion
% shocks. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INPUT VARIABLES: 
% Problem definition: Conditions at time t=0
%   rho1, u1, p1
%   rho4, u4, p4
% 'tEnd' and 'n' are the final solution time and the gas DoFs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global gamma

% Gamma values
alpha=(gamma+1)/(gamma-1);

% Assumed structure of exact solution
%
%    \         /      |con |       |s|
%     \   f   /       |tact|       |h|
% left \  a  /  state |disc| state |o| right
% state \ n /    2    |cont|   3   |c| state
%   1    \ /          |tinu|       |k|   4
%         |           |ity |       | |

PRL = p4/p1;
cright = sqrt(gamma*p4/rho4); 
cleft  = sqrt(gamma*p1/rho1);
CRL = cright/cleft;
MACHLEFT = (u1-u4)/cleft;

% Basic shock tube relation equation (10.51)
f = @(P) (1+MACHLEFT*(gamma-1)/2-(gamma-1)*CRL*(P-1)/sqrt(2*gamma*(gamma-1+(gamma+1)*P)))^(2*gamma/(gamma-1))/P-PRL;

% solve for P = p34 = p3/p4
p34 = fzero(f,3);

p3 = p34*p4;
rho3 = rho4*(1+alpha*p34)/(alpha+p34); 
rho2 = rho1*(p34*p4/p1)^(1/gamma);
u2 = u1-u4+(2/(gamma-1))*cleft*(1-(p34*p4/p1)^((gamma-1)/(2*gamma)));
c2 = sqrt(gamma*p3/rho2);
spos = 0.5+tEnd*cright*sqrt((gamma-1)/(2*gamma)+(gamma+1)/(2*gamma)*p34)+tEnd*u4;

x0 = 0.5;
conpos=x0 + u2*tEnd+tEnd*u4;	% Position of contact discontinuity
pos1 = x0 + (u1-cleft)*tEnd;	% Start of expansion fan
pos2 = x0 + (u2+u4-c2)*tEnd;	% End of expansion fan

% Plot structures
x = 0:0.002:1;
p = zeros(size(x)); 
ux= zeros(size(x)); 
rho = zeros(size(x));
Mach = zeros(size(x));  
cexact = zeros(size(x));

for i = 1:length(x)
    if x(i) <= pos1
        p(i) = p1;
        rho(i) = rho1;
        ux(i) = u1;
        cexact(i) = sqrt(gamma*p(i)/rho(i));
        Mach(i) = ux(i)/cexact(i);
    elseif x(i) <= pos2
        p(i) = p1*(1+(pos1-x(i))/(cleft*alpha*tEnd))^(2*gamma/(gamma-1));
        rho(i) = rho1*(1+(pos1-x(i))/(cleft*alpha*tEnd))^(2/(gamma-1));
        ux(i) = u1 + (2/(gamma+1))*(x(i)-pos1)/tEnd;
        cexact(i) = sqrt(gamma*p(i)/rho(i));
        Mach(i) = ux(i)/cexact(i);
    elseif x(i) <= conpos
        p(i) = p3;
        rho(i) = rho2;
        ux(i) = u2+u4;
        cexact(i) = sqrt(gamma*p(i)/rho(i));
        Mach(i) = ux(i)/cexact(i);
    elseif x(i) <= spos
        p(i) = p3;
        rho(i) = rho3;
        ux(i) = u2+u4;
        cexact(i) = sqrt(gamma*p(i)/rho(i));
        Mach(i) = ux(i)/cexact(i);
    else
        p(i) = p4;
        rho(i) = rho4;
        ux(i) = u4;
        cexact(i) = sqrt(gamma*p(i)/rho(i));
        Mach(i) = ux(i)/cexact(i);
    end
end
entro = log(p./rho.^gamma);	% entropy
e = p./((gamma-1).*rho);	% internal energy
t = p./rho; %2/n.*e;                 % temperature
