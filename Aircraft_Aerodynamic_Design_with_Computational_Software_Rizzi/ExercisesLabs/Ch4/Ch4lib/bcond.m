
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%             Alexander von Essen, Created Apr. 11 2005             %
%                   Last modified: May 10 2005
%                   MG additions Levmax, W, p, Nodes, a {1:Levmax}  %
%                      aug 2017                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: bcond.m                                                     %
% Purpose: Sets boundary conditions in dummy points.                %
% Called by: main.m solver.m                                        %
% Calls:                                                            %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function handles = bcond(handles)
%W,p = f(p01,t01,p2,gamma,R; Nodes,a,p,W)

p01   = handles.Data.p01;
t01   = handles.Data.t01;
p2    = handles.Data.p2;
gamma = handles.Data.gamma;
R     = handles.Data.R;
cpgas = R*gamma/(gamma-1);

ib2 = handles.Data.Nodes-1;
a   = handles.Data.a;
p   = handles.Data.p;
W   = handles.Data.W;

% Inlet.
u    = W(2,2)/W(2,1);
cs2  = gamma*p(2)*a(2)/W(2,1);
c02  = cs2 + 0.5*(gamma-1)*u^2;
rinv = u - 2*sqrt(cs2)/(gamma-1);
dis  = (gamma+1)*c02/((gamma-1)*rinv^2) - 0.5*(gamma-1);
if dis < 0
    dis = 1E-20;
end
cb   = -rinv*((gamma-1)/(gamma+1))*(1+sqrt(dis));
cc02 = min(cb^2/c02,1);
tb   = t01*cc02;
pb   = p01*(tb/t01)^(gamma/(gamma-1));
rhob = pb/(R*tb);
ub   = sqrt(2*cpgas*(t01-tb));
% JO 2021
rho01 = p01/t01/R;
Htot  = gamma/(gamma-1)*p01/rho01;
c     = (-rinv*(gamma-1) + sqrt((gamma^2-1)*(Htot*(gamma-1) + rinv^2)))/(gamma+1);
S     = p01/rho01^gamma;
%rhob  = (c^2/gamma/S)^1/(gamma-1);
%pb    = S*rhob^gamma;
%ub    = rinv+2*c/(gamma-1);
W(1,1) = rhob*a(2);
W(1,2) = rhob*a(2)*ub;
W(1,3) = (pb/(gamma-1)+0.5*rhob*ub^2)*a(2);
p(1)   = pb;


% Outlet.
rho = W(ib2,1)/a(ib2);
u   = W(ib2,2)/W(ib2,1);
cs  = sqrt(gamma*p(ib2)/rho);
% JO 2104 - check that end pressure > p2
if u >= cs & p(ib2) > p2
    % Supersonic.
    pb   = p(ib2);
    rhob = rho;
    ub   = u;
else
    % Subsonic.
    pb   = p2;
    rhob = rho + (p2-p(ib2))/cs^2;
    ub   = u - (p2-p(ib2))/(cs*rho);
end
W(end,1) = rhob*a(ib2);
W(end,2) = rhob*ub*a(ib2);
W(end,3) = (pb/(gamma-1)+0.5*rhob*ub^2)*a(ib2);
p(end)   = pb;

handles.Data.p = p;
handles.Data.W = W;
