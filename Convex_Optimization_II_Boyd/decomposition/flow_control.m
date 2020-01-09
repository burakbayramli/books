% FLOW_CONTROL.M
% Solve:
%
% maximize    sum(log(f))
% subject to  R*f <= c
%
% via dual decomposition

alpha=3;

m = 12; %number of links
n = 10; %number of flows

% Routing matrix
R = [0 0 1 0 0 1 0 0 0 0 0 1;
     1 0 0 1 0 0 0 1 1 0 0 0;
     0 0 1 0 0 0 0 0 1 0 1 0;
     0 0 0 0 1 0 1 0 0 0 0 1;
     1 1 0 0 0 0 1 0 0 0 1 0;
     0 0 0 0 0 1 0 1 0 1 0 0;
     0 0 1 0 0 0 0 1 0 1 0 0;
     1 1 0 1 1 0 0 0 0 0 0 0;
     0 0 0 1 0 1 0 0 1 1 0 0;
     1 0 0 0 1 0 0 1 0 0 1 0]';
 
% Capacity limits
rand('state',1);
c = 0.1 + 0.9*rand(m,1);

% Solve problem using cvx
if 0
cvx_begin
   variable f_star(n)
   dual variable lambda_star
   maximize(geomean(f_star))
   subject to
       lambda_star: R*f_star <= c
cvx_end
end
 
% Solve using dual decomposition
MAX_ITER = 100;
lambda = 1*ones(m,1);
prim_obj = []; dual_obj = []; infeas = [];
for i = 1:MAX_ITER
    % Flow updates
    f = 1./(R'*lambda);
    infeas = [infeas max(R*f-c)];    
    
    
    % Scale flows to feasibility
    eta = (R*f)./c;
    for j = 1:n
        f_feas(j,1) = (1/max(eta(find(R(:,j)))))*f(j);
    end
    prim_obj = [prim_obj sum(log(f_feas))];
    
    % Dual subgradient
    g = c-R*f;
    
    % Price update
    lambda = lambda - alpha*1*g;
    lambda(lambda<0) = 0;
    dual_obj = [dual_obj -sum(log(R'*lambda))+lambda'*c-n];
end

figure(1), clf
set(gca,'FontSize',16);
plot(1:MAX_ITER,prim_obj,'r-',1:MAX_ITER,dual_obj,'b--','LineWidth',1.5);
legend('primal','dual label')
xlabel('k'), ylabel('objective')
print -depsc flow_control_evol.eps

figure(2), clf
set(gca,'FontSize',16);
semilogy(1:MAX_ITER,infeas,'r-','LineWidth',1.5);
xlabel('k'), ylabel('infeasibility')
print -depsc max_cap_viol.eps
