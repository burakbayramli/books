 function [ke, fe] = elast2Delem(e)
include_flags;

ke  = zeros(nen*ndof,nen*ndof);     % initialize element stiffness
fe  = zeros(nen*ndof,1);            % initialize element force vector

% get coordinates of element nodes 
je = IEN(:,e);  
C  = [x(je); y(je)]';

[w,gp] = gauss(ngp);   % get gauss points and weights

% compute element stiffness matrix and element nodal force vector 
for i=1:ngp
   for j=1:ngp
       eta = gp(i);            
       psi = gp(j);
 
       N             = NmatElast2D(eta,psi);       % shape functions matrix  
       [B, detJ]     = BmatElast2D(eta,psi,C);     % derivative of the shape functions
       
       ke = ke + w(i)*w(j)*B'*D*B*detJ;   % element conductance matrix
       be = N*b(:,e);                     % interpolate body forces using element shape functions
       fe = fe + w(i)*w(j)*N'*be*detJ;    % element nodal force vector 

   end       
end





