function f = wall_bc(f, side)
% D2Q9
% Applies a wall bounceback BC to the distribution functions in 2d matrix form.
% Assumes boundary nodes are located on the boundary (as opposed to half-cell away).

if strcmp(side, 'north') % North wall.
    f(end,:,5) = f(end,:,3);
    f(end,:,9) = f(end,:,7);
    f(end,:,8) = f(end,:,6);
%     f(end,:,2) = 0;
%     f(end,:,4) = 0;
end
if strcmp(side, 'south') % South wall.
    f(1,:,3) = f(1,:,5);
    f(1,:,6) = f(1,:,8);
    f(1,:,7) = f(1,:,9);
%     f(1,:,2) = 0;
%     f(1,:,4) = 0;
end
if strcmp(side, 'east') % East wall.
    f(:,end,4) = f(:,end,2);
    f(:,end,8) = f(:,end,6);
    f(:,end,7) = f(:,end,9);
%     f(:,end,3) = 0;
%     f(:,end,5) = 0;
end
if strcmp(side, 'west') % West wall.
    f(:,1,2) = f(:,1,4);
    f(:,1,6) = f(:,1,8);
    f(:,1,9) = f(:,1,7);
%     f(:,end,3) = 0;
%     f(:,end,5) = 0;
end
    
    
    
        
    

