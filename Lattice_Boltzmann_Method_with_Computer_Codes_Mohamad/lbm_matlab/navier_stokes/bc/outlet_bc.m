function f = outlet_bc(f, side)
% D2Q9
% Applies an outlet BC to the distirbution functions in 2d matrix form.
% Assumes boundary nodes are located on the boundary (as opposed to half-cell away).

if strcmp(side, 'east') % East outlet (extrapolation).
    f(2:end-1,end,4) = f(2:end-1,end-1,4);
    f(2:end-1,end,8) = f(2:end-1,end-1,8);
    f(2:end-1,end,7) = f(2:end-1,end-1,7);
end

