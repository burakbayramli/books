function y = SpringAssemble(K,k,i,j)
%SpringAssemble   This function assembles the element stiffness
%                 matrix k of the spring with nodes i and j into the
%                 global stiffness matrix K.
%                 This function returns the global stiffness matrix K 
%                 after the element stiffness matrix k is assembled.
K(i,i) = K(i,i) + k(1,1);
K(i,j) = K(i,j) + k(1,2);
K(j,i) = K(j,i) + k(2,1);
K(j,j) = K(j,j) + k(2,2);
y = K;


