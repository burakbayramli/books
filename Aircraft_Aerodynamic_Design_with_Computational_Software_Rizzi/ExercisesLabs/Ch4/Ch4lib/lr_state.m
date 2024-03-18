%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%             Alexander von Essen, Created Apr. 5 2005              %
%                   Last modified: May 10 2005                      %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: lr_state.m                                                  %
% Purpose: Calculates (limited) left and right state using          %
%          MUSCL interpolation.                                     %
% Called by: solveexplicit.m                                        %
% Calls: muscl.m                                                    %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [handles] = lr_state(handles)
ib2 = handles.Data.Nodes - 1;
x = handles.Data.x;
a = handles.Data.a;
p = handles.Data.p;
dxref = handles.Data.dxref;
rhoref = handles.Data.rhoref;
uref = handles.Data.uref;
pref = handles.Data.pref;
limfac = handles.Data.limfac;
W      = handles.Data.W;
% JO 1905
gamma = handles.Data.gamma;
limprim = 1;
HiRes   = 1;
if ~limprim
    charW = U2W(W,a,gamma);
end

nnodes=ib2+1;

ls    = zeros(nnodes,3);
rs    = zeros(nnodes,3);

du   = zeros(nnodes+1,3);
ib22 = length(du)-1;

deltar = zeros(nnodes,3);
deltal = zeros(nnodes,3);

eps2    = zeros(3,1);
% deltl   = zeros(3,1);
% deltr   = zeros(3,1);

limfac3 = limfac^3;
rdxref  = 1/(dxref^3);
if limprim
    eps2(1) = limfac3*rhoref^2*rdxref; % rho
    eps2(2) = limfac3*uref^2*rdxref;   % u
    eps2(3) = limfac3*pref^2*rdxref;   % p
else
    eps2(1) = limfac3*uref^2*rdxref;                % u-2c/(g-1)
    eps2(2) = limfac3*(pref/rhoref^gamma)^2*rdxref; % p/rho^gamma
    eps2(3) = limfac3*uref^2*rdxref;                % u + 2c/(g-1)
end
if limprim
    du(2:ib22,1) = W(2:end,1)./a(2:end) - W(1:ib2,1)./a(1:ib2);
    du(2:ib22,2) = W(2:end,2)./W(2:end,1) - W(1:ib2,2)./W(1:ib2,1);
    du(2:ib22,3) = p(2:end) - p(1:ib2);
else
    du(2:ib22,:) = diff(charW);
end
du(1,:)   = du(2,:);
% du(1,2)   = du(2,2);
% du(1,3)   = du(2,3);
du(end,:) = du(ib22,:);
% du(end,2) = du(ib22,2);
% du(end,3) = du(ib22,3);

dx = diff(x);
eps2n = eps2(1).*dx.^3;
deltar(1:ib2,1) = HiRes*0.25*muscl(0,du(3:end  ,1),du(2:end-1,1),eps2n);
deltal(1:ib2,1) = HiRes*0.25*muscl(0,du(2:end-1,1),du(1:ib2  ,1),eps2n);

eps2n = eps2(2).*dx.^3;
deltar(1:ib2,2) = HiRes*0.25*muscl(0,du(3:end,2),du(2:end-1,2),eps2n);
deltal(1:ib2,2) = HiRes*0.25*muscl(0,du(2:end-1,2),du(1:ib2,2),eps2n);

eps2n = eps2(3).*dx.^3;
deltar(1:ib2,3) = HiRes*0.25*muscl(0,du(3:end,3),du(2:end-1,3),eps2n);
deltal(1:ib2,3) = HiRes*0.25*muscl(0,du(2:end-1,3),du(1:ib2,3),eps2n);
if limprim
    rs(1:ib2,1) = W(2:end,1)./a(2:end)   - deltar(1:ib2,1);
    rs(1:ib2,2) = W(2:end,2)./W(2:end,1) - deltar(1:ib2,2);
    rs(1:ib2,3) = p(2:end)               - deltar(1:ib2,3);
    
    ls(1:ib2,1) = W(1:ib2,1)./a(1:ib2)   + deltal(1:ib2,1);
    ls(1:ib2,2) = W(1:ib2,2)./W(1:ib2,1) + deltal(1:ib2,2);
    ls(1:ib2,3) = p(1:ib2)               + deltal(1:ib2,3);
else
    rs(1:ib2,:) = W2V(charW(2:end,:)-deltar(1:ib2,:),gamma);
    ls(1:ib2,:) = W2V(charW(1:ib2,:)+deltal(1:ib2,:),gamma);
end

handles.Data.ls = ls;
handles.Data.rs = rs;
