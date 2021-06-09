function [bp] = get_rhs(imax,jmax,dx,dy,rho,u_star,v_star)

bp=zeros((jmax-2)*(imax-2),1);    %vector of RHS for solving pressure corrections
stride = jmax;

% RHS is the same for all nodes except the p_prime(1,1)
% because p(1,1) is set to be zero, it has no pressure correction
for j=1:jmax
    for i=1:imax
        position = i + (j-1)*stride; 
        bp(position) = rho * (u_star(i,j)*dy - u_star(i+1,j)*dy + v_star(i,j)*dx - v_star(i,j+1)*dx); 
        
    end
end

% modify for p_prime(1,1)
bp(1,1) = 0;

return 
end
