% Compute material matrix (plane strain)
function C= CompCPlaneStrain(materials)
C=zeros(3,3);    % set up 3x3 empty matrix
E=materials(1);  % Young's modulus
nu=materials(2); % Poisson's ratio
t = E/((1+nu)*(1-2*nu));
C = t*[(1-nu)   nu     0
        nu     (1-nu)  0
        0       0      (1-2*nu)/2];