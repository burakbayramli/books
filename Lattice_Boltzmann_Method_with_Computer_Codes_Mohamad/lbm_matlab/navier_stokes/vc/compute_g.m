function g = compute_g(Fx,Fy,u,v)
% Fx and Fy are nodes x nodes, where the 3rd dimension applies to 
%   component of force.
% g is nodes x nodes x 9, where 3rd dimension corresponds to lattice vector
%   component.

% Constants.
w = zeros(9,1);
w(1) = 4/9;
w(2:5) = 1/9;
w(6:9) = 1/36;
c = zeros(9,2);
c(1,:) = [0, 0];
c(2,:) = [1, 0];
c(3,:) = [0, 1];
c(4,:) = [-1, 0];
c(5,:) = [0, -1];
c(6,:) = [1, 1];
c(7,:) = [-1, 1];
c(8,:) = [-1, -1];
c(9,:) = [1, -1];

[nodes,~] = size(u);
g = zeros(nodes,nodes,9);
for k = 1:9
    dot_term = c(k,1) * u + c(k,2) * v;
    term1 = Fx .* ( 3*(c(k,1)-u) + 9*dot_term*c(k,1) );
    term2 = Fy .* ( 3*(c(k,2)-v) + 9*dot_term*c(k,2) );
    g(:,:,k) = w(k) * ( term1 + term2 );
end
