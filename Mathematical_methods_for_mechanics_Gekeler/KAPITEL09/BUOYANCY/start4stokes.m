function [p,e,t,out_bound] = start4stokes(FF1,FF2,OPTION_MESH,REFINE) 
% makes mesh 
% E.W. Gekeler, Release 2.03.07 
switch OPTION_MESH
case 1, disp(' mesh without TOOLBOX, slow ')
   [p,e,t] = feval(FF1); % first mesh
   for J = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01_t(FF2,p,e,t);
     % pdemesh(p,e,t), hold on
      %  p       = mesh10(p,e,t,4); % Jigglemesh
      %  t       = mesh03(p,t,0);   % replace long edges
   end
   %pdemesh(p,e,t), pause
case 2, disp(' mesh with TOOLBOX, fast ')
 %  [p,e,t] =  initmesh(FF2,'hmax',inf,'Box','on');
   [p,e,t] =  initmesh(FF2,'hmax',inf);
  % [p,e,t] =  initmesh(FF2);
%   pdemesh(p,e,t), pause
   p       = jigglemesh(p,e,t,'Opt','minimum');
   for J = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = refinemesh(FF2,p,e,t,'regular');
      p       = jigglemesh(p,e,t,'Opt','minimum');  
   end
   %pdemesh(p,e,t), pause
end   
% -- Order boundary !!! -----------------------------
LL = max(e(5,:)); aux = [];
for I = 1:LL
   J = find(e(5,:) == I); EE = e(:,J);  
   [U,K] = sort(EE(3,:)) ; EE = EE(:,K);
   aux = [aux,EE];
end
e = aux;  
% -- boundary without inner segments ---------------
out_bound = find(e(7,:) == 0);
out_bound = sort(out_bound);
out_bound = e(:,out_bound);
