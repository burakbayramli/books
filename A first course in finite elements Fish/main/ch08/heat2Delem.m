 function [ke, fe] = heat2Delem(e)
include_flags;

ke  = zeros(nen,nen);    % initialized element conductance matrix
fe  = zeros(nen,1);      % initialize element nodal source vector

% get coordinates of element nodes 
je = IEN(:,e);  
C  = [x(je); y(je)]'; 

[w,gp] = gauss(ngp);    % get gauss points and weights

% compute element conductance matrix and nodal flux vector 
for i=1:ngp
   for j=1:ngp
       eta = gp(i);            
       psi = gp(j);
 
       N             = NmatHeat2D(eta,psi);       % shape functions matrix  
       [B, detJ]     = BmatHeat2D(eta,psi,C);     % derivative of the shape functions

       ke = ke + w(i)*w(j)*B'*D*B*detJ;   % element conductance matrix
       se = N*s(:,e);                     % compute s(x)
       fe = fe + w(i)*w(j)*N'*se*detJ;    % element nodal source vector

   end       
end




