function [RDU,RDV,RDT,RDP,FU,FV,FT,RDZ,RCZ] = bsp03h(p,e,p1,parmeter)
% Benard cell
% boundary conditions for Taylor-Hood-Element
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 

if nargin == 3, parmeter = []; end

SEGNR = parmeter(3:end); TEMP_U = parmeter(1); TEMP_O = parmeter(2);
SEGNR_A = SEGNR(1:12);  SEGNR_B = SEGNR(13);
SEGNR_C = SEGNR(14:25); SEGNR_D = SEGNR(26);

% -- BOUNDARY CONDITIONS for U and V
RDU_A = []; 
for I = 1:length(SEGNR_A)
    K  = find(e(5,:) == SEGNR_A(I)); LK = length(K);
    RDU_A = [RDU_A,[e(1,K),e(8,K); zeros(1,2*LK)]];   % left closed, right open
end
RDV_A = RDU_A;
RDU_B = []; 
for I = 1:length(SEGNR_B)
    K  = find(e(5,:) == SEGNR_B(I)); LK = length(K);
    RDU_B = [RDU_B,[e(1,K),e(8,K); zeros(1,2*LK)]];   % left closed, right open
end
RDV_B = RDU_B;

RDU_C = []; 
for I = 1:length(SEGNR_C)
    K  = find(e(5,:) == SEGNR_C(I)); LK = length(K);
    RDU_C = [RDU_C,[e(1,K),e(8,K); zeros(1,2*LK)]];   % left closed, right open
end
RDV_C = RDU_C;
RDU_D = []; 
for I = 1:length(SEGNR_D)
    K  = find(e(5,:) == SEGNR_D(I)); LK = length(K);
    RDU_D = [RDU_D,[e(1,K),e(8,K); zeros(1,2*LK)]];   % left closed, right open
end
RDV_D = RDU_D;

RDU = [RDU_A,RDU_B,RDU_C,RDU_D];
RDV = [RDV_A,RDV_B,RDV_C,RDV_D];

% -- Data for P  ------
%N1 = size(p,2);
RDP = [14;0]; % zero at midpoint of upper boundary

% -- DIRICHLET BOUNDARY CONDITIONS FOR TEMPERATUR
RDT_A = []; ;
for I = 1:length(SEGNR_A)
   K  = find(e(5,:) == SEGNR_A(I)); LK = length(K);
   RDT_A = [RDT_A,[e(1,K),e(8,K); TEMP_O*ones(1,2*LK)]];
end
RDT_B = []; ;
for I = 1:length(SEGNR_B)
   K  = find(e(5,:) == SEGNR_B(I)); LK = length(K);
   RDT_B = [RDT_B,[e(1,K),e(8,K); TEMP_O*ones(1,2*LK)]];
end
RDT_C = [];
for I = 1:length(SEGNR_C)
   K  = find(e(5,:) == SEGNR_C(I)); LK = length(K);
   RDT_C = [RDT_C,[e(1,K),e(8,K); TEMP_U*ones(1,2*LK)]];
end
RDT_D = []; ;
for I = 1:length(SEGNR_D)
   K  = find(e(5,:) == SEGNR_D(I)); LK = length(K);
   RDT_D = [RDT_D,[e(1,K),e(8,K); TEMP_O*ones(1,2*LK)]];
end

RDT = [RDT_A,RDT_B,RDT_C,RDT_D];
RCT = [];

% -- Loads
N = size(p,2) + size(p1,2);
FU = zeros(N,1); FV = FU; FT = FU;
% -- BOUNDARY CONDITIONS for stream function Z (for postprozessing)
RDZ = [];
for I = 1:length(SEGNR)
K  = find(e(5,:) == SEGNR(I)); LK = length(K); % Randsegment 1
   RDZ = [RDZ,[e(1,K),e(8,K); zeros(1,2*LK)]];
end
RCZ = [];
