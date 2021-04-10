function y = LinearBarElementForces(k,u)
%LinearBarElementForces   This function returns the element nodal 
%                         force vector given the element stiffness  
%                         matrix k and the element nodal displacement 
%                         vector u.
y = k * u;

