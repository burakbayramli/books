% B matrix function for 2D elasticity 
function [B, detJ] = BmatElast2D(eta,psi,C)

      % Calculate the Grad(N) matrix
        GN    = 0.25 * [eta-1  1-eta   1+eta   -eta-1;
                        psi-1  -psi-1  1+psi    1-psi];
        J     = GN*C;        % Compute Jacobian matrix 
        
        detJ  = det(J);     % Jacobian
      
        BB     = J\GN;       % compute the derivative of the shape functions
        
        B1x     = BB(1,1);
        B2x     = BB(1,2);
        B3x     = BB(1,3);
        B4x     = BB(1,4);
        B1y     = BB(2,1);
        B2y     = BB(2,2);
        B3y     = BB(2,3);
        B4y     = BB(2,4);
        
        
        
        B = [ B1x      0     B2x     0      B3x    0      B4x     0  ;
                0     B1y     0     B2y      0     B3y     0      B4y; 
              B1y     B1x    B2y    B2x     B3y    B3x    B4y     B4x];   
