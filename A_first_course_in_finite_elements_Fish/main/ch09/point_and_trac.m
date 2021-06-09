% compute and assemble nodal boundary force vector and point forces 
function f = point_and_trac(f);
include_flags;

% Assemble point forces
f(ID) = f(ID) + P(ID);


% Compute nodal boundary force vector
for i = 1:nbe
    
        ft      = [0 0 0 0]';       % initialize nodal boundary force vector
        node1   = n_bc(1,i);        % first node
        node2   = n_bc(2,i);        % second node
        n_bce   = n_bc(3:6,i);      % traction value at node1
                        
        x1 = x(node1); y1=y(node1);    % coord of the first node
        x2 = x(node2); y2=y(node2);    % coord of the second node
    
        leng = sqrt((x2-x1)^2 + (y2-y1)^2);  % edge length
        J    = leng/2;                       % Jacobian 
        
        [w,gp] = gauss(ngp);                % get gauss points and weights
       
        for i=1:ngp                         % integrate in psi direction (1D integration)  
               
            psi = gp(i);               
            N   = 0.5*[1-psi    0      1+psi      0;       % 1D shape functions x-component
                        0     1-psi      0      1+psi];    % 1D shape functions y-component  
                                 
            T    = N * n_bce;
            
            ft  = ft + w(i)*N' *T *J;      % nodal bounbdary force vector
        end

        % Assemble nodal boundary force vector
        ind1 = ndof*(node1-1)+1;       % pointer to first node
        ind2 = ndof*(node2-1)+1;       % pointer to second node
        
        f(ID(ind1))   = f(ID(ind1))   + ft(1) ;  
        f(ID(ind1+1)) = f(ID(ind1+1)) + ft(2) ;  
        f(ID(ind2))   = f(ID(ind2))   + ft(3) ;
        f(ID(ind2+1)) = f(ID(ind2+1)) + ft(4);
        
        
end    

    
    


                
                
                
        
