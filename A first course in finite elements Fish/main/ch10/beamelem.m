% generate the element stiffness matrix for each element
function [ke, fe] = beamelem(e)
include_flags;

IENe      = IEN(:,e);               % extract local connectivity information
xe        = x(IENe);                % extract x coordinates
J         = (xe(nen) - xe(1))/2;    % compute Jacobian 
[w , gp]  = gauss(ngp);             % extract Gauss points and weights 

ke = zeros(neqe,neqe);              % initialize element stiffness matrix
fe = zeros(neqe,1)   ;              % initialize element nodal force vector

for i = 1:ngp  
  N    = NmatrixBeam(gp(i),xe);           % shape functions matrix 
  B    = BmatrixBeam(gp(i),xe) *1/J^2;    % derivative of shape functions 
  Ae   = [N(1) N(3)]*CArea(IENe);         % calculate cross-sectional area at element gauss points   
  Ee   = E(e);                            % extract Young's modulus
  be   = body(e);                         % extract body forces  
  ke = ke + w(i)*(B'*Ae*Ee*B);            % calculate element stiffness matrix
  fe = fe + w(i)*N'*be;                   % calculate element nodal force vector
end
ke = J*ke;                            
fe = J*fe;


% check for point forces in this element
for i=1:np                % loop over all point forces
    Pi  = P(i);           % extract point force
    xpi = xp(i);          % extract the location of point force within an element
    if xe(1)<=xpi &  xpi<xe(nen)
        fe = fe + Pi*[NmatrixBeam( ( (2*xpi-xe(1)-xe(nen))/(xe(nen) - xe(1)) ) ,xe)]';     % add to element nodal force vector
    end
end
