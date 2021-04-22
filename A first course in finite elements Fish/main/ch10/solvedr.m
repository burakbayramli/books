% partition and solve the system of equations
function [d,f_E] = solvedr(K,f,d)
include_flags;

% partition the matrix K, vectors f and d
K_E	= K(1:nd,1:nd);                     	  % Extract K_E matrix 
K_F	= K(nd+1:neq,nd+1:neq);                   % Extract K_E matrix
K_EF    = K(1:nd,nd+1:neq);                   % Extract K_EF matrix
f_F  	= f(nd+1:neq);                        % Extract f_F vector
d_E  	= d(1:nd);                            % Extract d_E vector
 
% solve for d_F
d_F	=K_F\( f_F - K_EF'* d_E);
 
% reconstruct the global displacement d
d = [d_E             
     d_F];                
 
% compute the reaction f_E
f_E = K_E*d_E+K_EF*d_F-(f(1:nd));
 
% write to the workspace
solution_vector_d	= d
reactions_vector 	= f_E
