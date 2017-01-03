function [phi] = linear_building_blocks(x,X)

% number of nodes
N = length(x)-1;
for i=1:N-1
    phi(i,:) = ((1/(x(i+1)-x(i)))*(X-x(i+1)) + 1).*(X>=x(i)).*(X<=x(i+1))+((-1/(x(i+2)-x(i+1)))*(X-x(i+2))).*(X>x(i+1)).*(X<=x(i+2));
end
i = N;
phi(N,:) = ((1/(x(i+1)-x(i)))*(X-x(i+1)) + 1).*(X>=x(i)).*(X<=x(i+1));

    