function [RD,RC,LASTEN] = bsp002h(p,e,t)
% POISSON-Gleichung,   Randbedingungen

% -- DIRICHLET-Randbedingungen -------
I  = find(e(5,:) == 1); LI = length(I); % Boundary 1
E = [e(1,I), e(2,I(LI))];
RD1 = [E; zeros(1,LI+1)];
I  = find(e(5,:) == 2); LI = length(I); % Boundary 2
E = [e(1,I), e(2,I(LI))];
RD2 = [E; zeros(1,LI+1)];
I  = find(e(5,:) == 3); LI = length(I); % Boundary 3
E = [e(1,I), e(2,I(LI))];
RD3 = [E; zeros(1,LI+1)];
I  = find(e(5,:) == 4); LI = length(I); % Boundary 4
E = [e(1,I), e(2,I(LI))];
RD4 = [E; zeros(1,LI+1)];
% Dirichlet-Rand Auswaehlen! ---------
RD = [RD1, RD3];
% homogene Dirchlet-Randbedingungen:
RD = [e(1,:);zeros(1,size(e,2))];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- CAUCHY-Randbedingungen ------------
% -- Beispiel:
%I  = find(e(5,:) == 1); LI = length(I); % Boundary 1
%BETA = 0; GAMMA = 0;
%RC1 = [e(1:2,I); BETA*ones(1,LI); GAMMA*ones(1,LI)];

% -- CAUCHY-Randbedingungen Auswaehlen! -------
RC = [];
% -- LASTEN (Rechte Seite) ------------
% entweder Skalar oder ZEILENVEKTOR ----
LASTEN = 10;
