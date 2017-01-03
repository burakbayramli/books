function [phi,X] = quartic_building_blocks(x)
% note: these only work for equally-spaced nodes

% number of nodes
N = length(x)-1;
% make a longer vector, one that fills in between each node

dx = x(2)-x(1);
X = x(1):dx/10:x(N+1);

for i=1:N-1
  phi(i,:) = ((X-x(i)).^2.*(X-x(i+2)).^2)/(dx^4).*(X>=x(i)).*(X<=x(i+2));
end
i = N;
phi(N,:) = ((X-x(i)).^2.*(X-(x(i+1)+dx)).^2)/(dx^4).*(X>=x(i));

    
