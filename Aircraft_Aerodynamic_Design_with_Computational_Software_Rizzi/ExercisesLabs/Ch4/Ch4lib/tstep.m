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
function handles = tstep(handles)
% dt = f(gamma; a,vol,p,W)
gamma = handles.Data.gamma;
%
    a   = handles.Data.a;
    p   = handles.Data.p;
    vol = handles.Data.vol;
    W   = handles.Data.W;
    rho = W(:,1)./a;
    u   = W(:,2)./W(:,1);
    cs  = sqrt(gamma*p./rho);
    spr = (cs+abs(u)).*a;
    % JO 1904
    if norm(a-mean(a))<=0.001 % shock tube: time accurate
                              % still, time steps may vary with time
        handles.Data.dt = ones(size(rho))*min(vol./spr);
    else % steady nozzle
        handles.Data.dt = vol./spr;
    end
