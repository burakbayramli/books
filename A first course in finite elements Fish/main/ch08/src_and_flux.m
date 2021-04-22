% Compute and assemble nodal boundary flux vector and point sources 

function f = src_and_flux(f);
include_flags;

% Assemble point sources 
f(ID) = f(ID) + P(ID);


% Compute nodal boundary flux vector
for i = 1:nbe
    
        fq      = [0 0]';            % initialize the nodal source vector
        node1   = n_bc(1,i);         % first node
        node2   = n_bc(2,i);         % second node
        n_bce   = n_bc(3:4,i);       % flux value at an edge
        
        
        x1 = x(node1); y1=y(node1);    % coord of the first node
        x2 = x(node2); y2=y(node2);    % coord of the second node
    
        leng = sqrt((x2-x1)^2 + (y2-y1)^2);  % edge length
        J    = leng/2;                       % 1D Jacobian 
    
        
        [w,gp] = gauss(ngp);                % get gauss points and weights
       
        for i=1:ngp                         % integrate in psi direction (1D integration)  
               
            psi = gp(i);               
            N   = 0.5*[1-psi  1+psi];       % 1D  shape functions in parent domain
            
            flux = N * n_bce;
            fq      = fq + w(i)*N' *flux*J;      % nodal flux
        end
        fq = -fq;  % define flux as negative integrals
        
        % Assemble nodal flux vector
        f(ID(node1)) = f(ID(node1)) + fq(1) ;  
        f(ID(node2)) = f(ID(node2)) + fq(2);
        
        
end    

    
    


                
                
                
        
