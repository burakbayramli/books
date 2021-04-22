function k = SpaceTrussElement(e, A, coord)
% k = SpaceTrussElement(e, A, coord)
% Generates stiffness matrix of a space truss element
% e = modulus of elasticity
% A = Area of cross-section
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2); z1=coord(1,3);
x2=coord(2,1); y2=coord(2,2); z2=coord(2,3);
L=sqrt((x2-x1)^2+(y2-y1)^2+(z2-z1)^2);
ls=(x2-x1)/L; ms=(y2-y1)/L; ns=(z2-z1)/L;
k = e*A/L*[ls^2, ls*ms, ls*ns, -ls^2, -(ls*ms), -(ls*ns);
    ls*ms, ms^2, ms*ns, -(ls*ms), -ms^2, -(ms*ns);
    ls*ns, ms*ns, ns^2, -(ls*ns), -(ms*ns), -ns^2;
    -ls^2, -(ls*ms), -(ls*ns), ls^2, ls*ms, ls*ns;
    -(ls*ms), -ms^2, -(ms*ns), ls*ms, ms^2, ms*ns;
    -(ls*ns), -(ms*ns), -ns^2, ls*ns, ms*ns, ns^2];