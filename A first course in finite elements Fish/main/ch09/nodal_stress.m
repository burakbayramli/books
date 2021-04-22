% compute the average nodal stresses 
function nodal_stress(d,e);
include_flags;

de      = d(LM(:,e));    % element nodal displacements

% get coordinates of element nodes 
je  = IEN(:,e); 
C   = [x(je); y(je)]'; 

psi_val = [-1 1 1 -1];  % psi values at the nodes
eta_val = [-1 -1 1 1];  % eta values at the nodes

% compute strains and stresses at the element nodes
ind = 1;
for i=1:nen     
      eta = eta_val(i);  
      psi = psi_val(i);
  
       [B, detJ]     = BmatElast2D(eta,psi,C);
       
       strain(:,ind) = B*de;
       stress(:,ind) = D*strain(:,ind);       % compute the stress [s_xx  s_yy  s_xy]; 
       
       ind      = ind + 1;
end
e_xx = strain(1,:);  e_yy = strain(2,:);  e_xy = strain(3,:);     % strains at gauss points
s_xx = stress(1,:);  s_yy = stress(2,:);  s_xy = stress(3,:);     % stress at gauss points

counter(je)      = counter(je) + ones(nen,1);    % count the time a stress is added to a node     
nodestress(je,:) = [s_xx'    s_yy'    s_xy' ];   % add the stresses to the appropriate nodes 











