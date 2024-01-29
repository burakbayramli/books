% Compute material matrix (3-D)
function C= CompC3D(materials)
C=zeros(6,6);
E=materials(1);  % Young's modulus
nu=materials(2); % Poisson's ratio
t = E/((1+nu)*(1-2*nu));
c11=t*(1-nu);
c12=t*nu;
c44=t*((1-2*nu)/2);
C=[c11  c12  c12    0   0   0
   c12  c11  c12    0   0   0
   c12  c12  c11    0   0   0
     0    0    0  c44   0   0
     0    0    0    0 c44   0
     0    0    0    0   0 c44];