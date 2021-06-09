% generate the element stiffness matrix and element force vector for each element
function [ke, fe] = barelem(e);
include_flags;

IENe      = IEN(:,e);               % extract local connectivity information
xe        = x(IENe);                % extract x coordinates
J         = (xe(nen) - xe(1))/2;    % compute Jacobian 
[w , gp]  = gauss(ngp);             % Gauss points and weights 

ke = zeros(nen,nen);                % initialize element stiffness matrix
fe = zeros(nen,1);                  % initialize element nodal force vector
 
for i = 1:ngp  
  xt   = 0.5*(xe(1)+xe(nen))+J*gp(i);   % compute gauss points in the physical coordinates
 
  N    = Nmatrix1D(xt,xe);                % shape functions matrix 
  B    = Bmatrix1D(xt,xe);                % derivative of shape functions 

  Ae   = N*CArea(IENe);                 % calculate cross-sectional area at element gauss points   
  Ee   = N*E(IENe);                     % calculate Young's modulus at element gauss points    
  be   = N*body(IENe);                  % calculate body forces at element gauss points    
  ke = ke + w(i)*(B'*Ae*Ee*B);          % calculate element stiffness
  fe = fe + w(i)*N'*be;                 % calculate element nodal force vector
end
ke = J*ke;                              
fe = J*fe;

% check for point forces in this element
for i=1:np                % loop over all point forces
    Pi  = P(i);           % extract point force
    xpi = xp(i);          % extract the location of point force within an element
    if xe(1)<=xpi &  xpi<xe(nen)             
        fe = fe + Pi*[Nmatrix1D(xpi,xe)]';     % add to element nodal force vector
    end
end

