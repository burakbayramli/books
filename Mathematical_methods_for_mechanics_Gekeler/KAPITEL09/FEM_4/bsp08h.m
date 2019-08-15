function [RDZ,RCZ,RDW,RDT,RCT] = bsp08h_a(p,e,t,SEGNR,Parmeter)
% Benard cell
TEMP_U = Parmeter(7); TEMP_O = Parmeter(8);

SEGNR_A = SEGNR(1:12);  SEGNR_B = SEGNR(13);
SEGNR_C = SEGNR(14:25); SEGNR_D = SEGNR(26);

% -- BOUNDARY CONDITIONS FOR STREAM FUNCTION
RDZ = [];
for I = 1:length(SEGNR)
K  = find(e(5,:) == SEGNR(I)); LK = length(K); % Randsegment 1
   RDZ = [RDZ,[e(1,K); zeros(1,LK)]];
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
RCZ = [];
RDW = [];
for I = 1:length(SEGNR)
K  = find(e(5,:) == SEGNR(I)); LK = length(K); % Randsegment K
   RDW = [RDW,[e(1,K); ones(1,LK);zeros(1,LK)]];
end

% -- DIRICHLET BOUNDARY CONDITIONS FOR TEMPERATUR
RDT_A = []; ;
for I = 1:length(SEGNR_A)
K  = find(e(5,:) == SEGNR_A(I)); LK = length(K);
   RDT_A = [RDT_A,[e(1,K); TEMP_O*ones(1,LK)]];
end
RDT_C = [];
for I = 1:length(SEGNR_C)
K  = find(e(5,:) == SEGNR_C(I)); LK = length(K);
   RDT_C = [RDT_C,[e(1,K); TEMP_U*ones(1,LK)]];
end
RDT = [RDT_A,RDT_C];
RCT = [];
