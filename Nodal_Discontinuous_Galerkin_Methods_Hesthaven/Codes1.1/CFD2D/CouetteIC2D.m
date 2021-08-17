function [Q] = CouetteIC2D(x, y, time)

% function [Q] = CouetteIC2D(x, y, time)
% Purpose: evaluat solution for Couette flow

gamma = 1.4;

rad = sqrt(x.^2 + y.^2); theta = atan2(y, x);

Q = zeros(size(x,1), size(x,2), 4);

utheta = (-rad + 16./rad)/75;
p = 1 + (1./(75^2))*( (rad.^2)/2 - 32*log(rad) - 128./rad.^2 );

Q(:,:,1) = 1;
Q(:,:,2) = -sin(theta).*utheta;
Q(:,:,3) =  cos(theta).*utheta;
Q(:,:,4) = p/(gamma-1) + 0.5*(Q(:,:,2).^2 + Q(:,:,3).^2)./Q(:,:,1);
return
