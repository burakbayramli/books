function [u,v,rho] = reconstruct_macro(f,u,v)
% D2Q9
% Reconstructs the macroscale variables from the mesoscale variables.
% Only on the interior, not the BCs.

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

rho = sum(f,3);

% % Use entire matrices.
% [rows, cols] = size(rho);
% u = zeros(rows,cols);
% v = zeros(rows,cols);
% for k = 1:9
%     u = u + c(k,1)*f(:,:,k);
%     v = v + c(k,2)*f(:,:,k);
% end
% u = u ./ rho;
% v = v ./ rho;

% Use only interior points.
u(2:end-1,2:end-1) = 0;
v(2:end-1,2:end-1) = 0;
for k = 1:9
    u(2:end-1,2:end-1) = u(2:end-1,2:end-1) + c(k,1)*f(2:end-1,2:end-1,k);
    v(2:end-1,2:end-1) = v(2:end-1,2:end-1) + c(k,2)*f(2:end-1,2:end-1,k);
end
u(2:end-1,2:end-1) = u(2:end-1,2:end-1) ./ rho(2:end-1,2:end-1);
v(2:end-1,2:end-1) = v(2:end-1,2:end-1) ./ rho(2:end-1,2:end-1);
