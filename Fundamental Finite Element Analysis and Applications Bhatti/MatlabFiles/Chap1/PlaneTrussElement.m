function k = PlaneTrussElement(e, A, coord)
% PlaneTrussElement(e, A, coord)
% Generates stiffness matrix for a plane truss element
% e = modulus of elasticity
% A = area of cross-section
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
L=sqrt((x2-x1)^2+(y2-y1)^2);
ls=(x2-x1)/L; ms=(y2-y1)/L;
k = e*A/L*[ls^2, ls*ms,-ls^2,-ls*ms;
    ls*ms, ms^2,-ls*ms,-ms^2;
    -ls^2,-ls*ms,ls^2,ls*ms;
    -ls*ms,-ms^2,ls*ms,ms^2];