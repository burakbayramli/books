function f = moving_wall_bc_all(f, side, u_lb)
% D2Q9
% Applies a velocity to enforce a moving wall (as in lid-driven cavity).

if strcmp(side, 'north') % North boundary (moving lid).
    rho_end = f(end,:,1) + f(end,:,2) + f(end,:,4) + ...
        2*( f(end,:,3) + f(end,:,7) + f(end,:,6) );
    f(end,:,5) = f(end,:,3);
    f(end,:,9) = f(end,:,7) + (u_lb / 6)*rho_end;
    f(end,:,8) = f(end,:,6) - (u_lb / 6)*rho_end;
end 
    
    