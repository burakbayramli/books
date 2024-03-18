%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%             Alexander von Essen, Created Apr. 1 2005              %
%                   Last modified: May 10 2005                      %
%                MG additions: Levmax, a, vol, p, W {1:Levmax}  
%                aug 2017                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name:      tstep.m                                                %
% Purpose:   Calculates local time steps.                           %
% Called by: solveexplicit.m                                        %
% Calls:                                                            %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function handles = tstep_MG(handles)
levmax = handles.Data.Levmax;
% dt = f(gamma; a,vol,p,W)
gamma = handles.Data.gamma;
%
for k=1:levmax
    a   = handles.Data.a{k};
    p   = handles.Data.p{k};
    vol = handles.Data.vol{k};
    W   = handles.Data.W{k};
    rho = W(:,1)./a;
    u   = W(:,2)./W(:,1);
    cs  = sqrt(gamma*p./rho);
    spr = (cs+abs(u)).*a;
    handles.Data.dt{k} = vol./spr;
end
