%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                   %
%%                         DEMOFLOW                                  %
%%                                                                   %
%%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%%       ======================================================      %
%%             Alexander von Essen, Created Apr. 5 2005              %
%%                   Last modified: May 2 2005                       %
%%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                   %
%% Name: aroe.m                                                      %
%% Purpose: Assembles roe matrix for every node.                     %
%% Called by:                                                        %
%% Calls:                                                            %
%%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [AA] = aroe(p,rho,u,h,gamma)

E = h - (p(2:end)./rho);

phi = (0.5*(gamma - 1)).*u.^2;
a1 = gamma.*E - phi;
a2 = (gamma - 1).*ones(size(a1));
a3 = (gamma - 2).*ones(size(a1));

AA = zeros(3,3,length(a1));

AR = [[zeros(size(a1)) ones(size(a1)) zeros(size(a1))];
    [(phi - u.^2) (u - a3.*u) a2];
    [(u.*(phi - a1)) (a1 - a2.*u.^2) (gamma.*u)]];

ll = length(a1);
for n = 1:ll
    AA(:,:,n) = [AR(1,:); AR((n+ll),:); AR((n+(2*ll)),:)];
end