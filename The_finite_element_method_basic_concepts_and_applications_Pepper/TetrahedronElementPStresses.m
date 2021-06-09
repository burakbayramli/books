function y = TetrahedronElementPStresses(sigma)
%TetrahedronElementPStresses   This function returns the three 
%                              principal stresses for the element 
%                              given the element stress vector.
%                              The principal angles are not returned.
s1 = sigma(1) + sigma(2) + sigma(3);
s2 = sigma(1)*sigma(2) + sigma(1)*sigma(3) + sigma(2)*sigma(3) - sigma(4)*sigma(4) -sigma(5)*sigma(5) -sigma(6)*sigma(6);
ms3 = [sigma(1) sigma(4) sigma(6) ; sigma(4) sigma(2) sigma(5) ; sigma(6) sigma(5) sigma(3)];
s3 = det(ms3);
y = [s1 ; s2 ; s3];


