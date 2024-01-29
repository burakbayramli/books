%---------------------------------------
% Compute material matrix (plane stress)
%---------------------------------------

function C= CompCPlaneStress(materials)

  C=zeros(3,3);
  E=materials(1);  % Young's modulus
  nu=materials(2); % Poisson's ratio

  t = E/(1-(nu*nu));
  C(1,1)=t;
  C(2,2)=t;
  C(1,2)=t*nu;
  C(2,1)=t*nu;
  C(3,3)=t*((1-nu)/2);
