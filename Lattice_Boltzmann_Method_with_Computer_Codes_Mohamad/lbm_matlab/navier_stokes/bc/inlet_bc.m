function f = inlet_bc(f, u_lb, side)
% D2Q9
% Applies an inlet BC to the distirbution functions in 2d matrix form.
% Assumes boundary nodes are located on the boundary (as opposed to half-cell away).

if strcmp(side, 'west') % West inlet, fixed velocity.    
    rho_west = ( 1 / ( 1 - u_lb ) ) * ...
        ( f(2:end-1,1,1) + f(2:end-1,1,3) + f(2:end-1,1,5) + ...
        2*( f(2:end-1,1,4) + f(2:end-1,1,7) + f(2:end-1,1,8) ) );
    f(2:end-1,1,2) = f(2:end-1,1,4) + 2 / 3 * u_lb * rho_west;
    f(2:end-1,1,6) = f(2:end-1,1,8) + u_lb / 6 * rho_west;
    f(2:end-1,1,9) = f(2:end-1,1,7) + u_lb / 6 * rho_west;
end

