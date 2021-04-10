function y = QuadraticBarAssemble(K,k,i,j,m)
%QuadraticBarAssemble   This function assembles the element stiffness
%                       matrix k of the quadratic bar with nodes i, j 
%                       and m into the global stiffness matrix K.
%                       This function returns the global stiffness  
%                       matrix K after the element stiffness matrix  
%                       k is assembled.
K(i,i) = K(i,i) + k(1,1);
K(i,j) = K(i,j) + k(1,2);
K(i,m) = K(i,m) + k(1,3);
K(j,i) = K(j,i) + k(2,1);
K(j,j) = K(j,j) + k(2,2);
K(j,m) = K(j,m) + k(2,3);
K(m,i) = K(m,i) + k(3,1);
K(m,j) = K(m,j) + k(3,2);
K(m,m) = K(m,m) + k(3,3);
y = K;



