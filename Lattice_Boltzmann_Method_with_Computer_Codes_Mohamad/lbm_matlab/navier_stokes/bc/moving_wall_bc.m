function f = moving_wall_bc(f, side, u_lb)
% D2Q9
% Applies a velocity to enforce a moving wall (as in lid-driven cavity).

if strcmp(side, 'north') % North boundary (moving lid).
    rho_end = f(end,2:end-1,1) + f(end,2:end-1,2) + f(end,2:end-1,4) + ...
        2*( f(end,2:end-1,3) + f(end,2:end-1,7) + f(end,2:end-1,6) );
    f(end,2:end-1,5) = f(end,2:end-1,3);
    f(end,2:end-1,9) = f(end,2:end-1,7) + (u_lb / 6)*rho_end;
    f(end,2:end-1,8) = f(end,2:end-1,6) - (u_lb / 6)*rho_end;
end 
    
if strcmp(side, 'west') % North boundary (moving lid).
    rho_end = f(2:end-1,end,1) + f(2:end-1,end,3) + f(2:end-1,end,5) + ...
        2*( f(2:end-1,end,4) + f(2:end-1,end,7) + f(2:end-1,end,8) );
    f(2:end-1,end,2) = f(2:end-1,end,4);
    f(2:end-1,end,9) = f(2:end-1,end,7) + (u_lb / 6)*rho_end;
    f(2:end-1,end,6) = f(2:end-1,end,8) - (u_lb / 6)*rho_end;
end 
    