% Shape function for 2D elasticity
function N = NmatElast2D(eta,psi)
       

N1 = 0.25*(1-psi)*(1-eta);
N2 = 0.25*(1+psi)*(1-eta);
N3 = 0.25*(1+psi)*(1+eta);
N4 = 0.25*(1-psi)*(1+eta);

N  =  [N1   0    N2    0    N3   0    N4   0;    
       0    N1   0     N2   0    N3    0   N4];
