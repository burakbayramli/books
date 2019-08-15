function [p,e,t,RAND,INNERPKTE] = prepar(FF1,REFINE,SEGNR)
[p,e,t] =  initmesh(FF1,'hmax',inf);
p       = jigglemesh(p,e,t,'Opt','minimum');
for J = 1:REFINE
   [p,e,t] = refinemesh(FF1,p,e,t,'regular');
   p       = jigglemesh(p,e,t,'Opt','minimum');
end
% -- Order boundary !!! -----------------------------
LL = max(e(5,:)); f = [];
for I = 1:LL
   J = find(e(5,:) == I); EE = e(:,J);
   [U,K] = sort(EE(3,:)) ; EE = EE(:,K);
   f = [f,EE];
end
e = f;
% -- Order boundary without inner segments ---------------
RAND = [];
if ~isempty(SEGNR)
   for I = 1:length(SEGNR)
      J  = find(e(5,:) == SEGNR(I)); EE = e(:,J);
      [U,K] = sort(EE(3,:)); EE = EE(:,K);
      RAND = [RAND,EE];
   end
end
% -- Innere Punkte --------------
LP = size(p,2); AUX = zeros(1,LP);
for I = 1:LP
   if isempty(find(e(1,:) == I)), AUX(I) = 1; end
end
INNERPKTE = find(AUX == 1);

