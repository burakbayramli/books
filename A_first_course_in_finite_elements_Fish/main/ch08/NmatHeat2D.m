% Shape function 
 
function N = NmatHeat2D(eta,psi)
       
        N  = 0.25 * [(1-psi)*(1-eta)  (1+psi)*(1-eta)  (1+psi)*(1+eta)  (1-psi)*(1+eta)]; % shape functions

