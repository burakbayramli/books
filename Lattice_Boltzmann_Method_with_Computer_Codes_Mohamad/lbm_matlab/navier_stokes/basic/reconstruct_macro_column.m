function [u,v] = reconstruct_macro_column(f,u,v,side,ends)
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
if strcmp(side,'east')
    if ~ends
        u(2:end-1,end) = 0;
        v(2:end-1,end) = 0;
        for k = 1:9
            u(2:end-1,end) = u(2:end-1,end) + c(k,1)*f(2:end-1,end,k);
            v(2:end-1,end) = v(2:end-1,end) + c(k,2)*f(2:end-1,end,k);
        end
        u(2:end-1,end) = u(2:end-1,end) ./ rho(2:end-1,end);
        v(2:end-1,end) = v(2:end-1,end) ./ rho(2:end-1,end);
    else
        u(:,end) = 0;
        v(:,end) = 0;
        for k = 1:9
            u(:,end) = u(:,end) + c(k,1)*f(:,end,k);
            v(:,end) = v(:,end) + c(k,2)*f(:,end,k);
        end
        u(:,end) = u(:,end) ./ rho(:,end);
        v(:,end) = v(:,end) ./ rho(:,end);
    end
end