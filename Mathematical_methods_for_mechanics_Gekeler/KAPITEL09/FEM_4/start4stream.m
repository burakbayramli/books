function [p,e,t,BOUNDARY,INNERPKTE] = start4stream(FF1,FF2,OPTION,REFINE,SEGNR,SEGNR1) 
% makes mesh 
% E.W. Gekeler, Release 2.03.07 
switch OPTION
case 1, disp(' mesh without TOOLBOX, slow ')
   [p,e,t] = feval(FF1); % first mesh
   for J = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01_t(FF2,p,e,t);
     %   p       = mesh10(p,e,t,4); % Jigglemesh
     %   t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
   end
case 2, disp(' mesh with TOOLBOX, fast ')
   [p,e,t] =  initmesh(FF2,'hmax',inf);
%   [p,e,t] =  initmesh(FF2,'hmax',inf,'Box','on','Init','on');
   p       = jigglemesh(p,e,t,'Opt','minimum');
   for J = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = refinemesh(FF2,p,e,t,'regular');
      p       = jigglemesh(p,e,t,'Opt','minimum');
   end
   %pdemesh(p,e,t)
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
BOUNDARY = [];
if ~isempty(SEGNR)
   for I = 1:length(SEGNR)
      J  = find(e(5,:) == SEGNR(I)); EE = e(:,J);
      [U,K] = sort(EE(3,:)); EE = EE(:,K);
      BOUNDARY = [BOUNDARY,EE];
   end
end
if nargin == 6
   for I = 1:length(SEGNR1)
      J  = find(e(5,:) == SEGNR1(I)); EE = e(:,J);
      %[U,K] = sort(EE(3,:)); EE = EE(:,K);
      RAND = [RAND,EE];
   end
end
% -- Interior points --------------
LP = size(p,2); AUX = zeros(1,LP);
for I = 1:LP
   if isempty(find(e(1,:) == I)), AUX(I) = 1; end
end
INNERPKTE = find(AUX == 1);
