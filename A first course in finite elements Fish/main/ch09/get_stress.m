% compute the strains and stress at element gauss points
function get_stress(d,e);
include_flags;

de = d(LM(:,e));    % extract element nodal displacements

% get coordinates of element nodes 
je = IEN(:,e);  
C  = [x(je); y(je)]'; 


[w,gp] = gauss(ngp);   % get gauss points and weights

% compute strains and stress at the gauss points 
ind = 1;
for i=1:ngp
   for j=1:ngp
       eta = gp(i);  psi = gp(j);
       

      
       N             = NmatElast2D(eta,psi);
       [B, detJ]     = BmatElast2D(eta,psi,C);

       
       Na            = [N(1,1) N(1,3) N(1,5) N(1,7)];
       X(ind,:) =  Na*C;                      % gauss points in physical coordinates 
       strain(:,ind) = B*de;
       stress(:,ind) = D*strain(:,ind);       % compute the stresses [s_xx  s_yy  s_xy]; 
       
       ind      = ind + 1;
   end
end
e_xx = strain(1,:);  e_yy = strain(2,:);  e_xy = strain(3,:);     % strains at gauss points
s_xx = stress(1,:);  s_yy = stress(2,:);  s_xy = stress(3,:);     % stresses at gauss points



%                 x-coord     y-coord    sigma_xx   sigma_yy    sigma_xy
stress_gauss    = [X(:,1)       X(:,2)    s_xx'         s_yy'   s_xy'  ];
fprintf(1,'\tx-coord\t\t\ty-coord\t\t\ts_xx\t\t\ts_yy\t\t\ts_xy\n');
fprintf(1,'\t%f\t\t%f\t\t%f\t\t%f\t\t%f\n',stress_gauss');






