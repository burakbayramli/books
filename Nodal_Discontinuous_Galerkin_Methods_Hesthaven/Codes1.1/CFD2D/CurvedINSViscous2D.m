% compute right hand side for viscous step solves
mmUxTT = (cub.V')*(cub.W.*(cub.V*UxTT));  Uxrhs = g0*mmUxTT(:)/(nu*dt) + rhsbcUx(:); 
mmUyTT = (cub.V')*(cub.W.*(cub.V*UyTT));  Uyrhs = g0*mmUyTT(:)/(nu*dt) + rhsbcUy(:);

% save Ux,Uy
Uxold = Ux; Uyold = Uy;

% viscous solves (RCM, backsolve twice, undo RCM)
Uxrhs = Uxrhs(VELperm); tmp = VELsystemCT\Uxrhs(:); Ux(VELperm) = VELsystemC\tmp; 
Uyrhs = Uyrhs(VELperm); tmp = VELsystemCT\Uyrhs(:); Uy(VELperm) = VELsystemC\tmp; 

