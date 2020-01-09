% supply_mpc.m
% supply chain management using MPC
% EE364b, Convex Optimization II, S. Boyd, Stanford University.
% Written by Yang Wang, 04/2008

% problem data
n = 5; m = 9;
Ain = [...
        0, 0, 0, 0, 0, 0, 0, 0, 0;
        0, 1, 0, 0, 1, 0, 0, 0, 0;
        0, 0, 1, 0, 0, 0, 1, 0, 0;
        1, 0, 0, 1, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 1, 0, 0, 0;
]; 
Aout = [...
        1, 1, 1, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 1, 0, 0, 0;
        0, 0, 0, 1, 1, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 1, 0;
        0, 0, 0, 0, 0, 0, 1, 0, 1;
]; 
x0 = [1;0;0;1;1];
xmax = ones(n,1); umax = 0.05*ones(m,1); lambda = 10;
Q = eye(n); q = ones(n,1); r = [ones(m-2,1);-1;-1];
Qhalf = sqrtm(Q);

% MPC
T = 5; % horizon
nsteps = 50; % number of steps
x = x0; Xall = zeros(n,nsteps); Uall = zeros(m,nsteps);
for i = 1:nsteps
    cvx_precision(max(min(x)/10,1e-6))
    cvx_begin
        variables X(n,T+1) U(m,T)
        max(X') <= xmax'; min(X') >= 0;
        max(U') <= umax'; min(U') >= 0;
        Aout*U - X(:,1:T) <= 0;
        X(:,2:T+1) == X(:,1:T)+Ain*U-Aout*U;
        X(:,1) == x;
        minimize (sum(sum_square(Qhalf*X(:,2:T+1)))...
                 +sum(q'*X(:,2:T+1))+sum(r'*U)+lambda*sum(X(:,T+1)))
    cvx_end
    Xall(:,i) = x; u = U(:,1); Uall(:,i) = u;
    x = x+Ain*u-Aout*u; 
end
supplympccost = sum(sum_square(Qhalf*Xall))+sum(q'*Xall)+sum(r'*Uall);

% optimal
Topt = 50;
cvx_begin
    variables Xopt(n,Topt+1) Uopt(m,Topt)
    max(Xopt') <= xmax'; min(Xopt') >= 0;
    max(Uopt') <= umax'; min(Uopt') >= 0;
    Aout*Uopt - Xopt(:,1:Topt) <= 0;
    Xopt(:,2:Topt+1) == Xopt(:,1:Topt)+Ain*Uopt-Aout*Uopt;
    Xopt(:,1) == x0;
    minimize (sum(sum_square(Qhalf*Xopt(:,2:Topt+1)))...
             +sum(q'*Xopt(:,2:Topt+1))+sum(r'*Uopt)+lambda*sum(Xopt(:,Topt+1)))
cvx_end
supplyoptcost = sum(sum_square(Qhalf*Xopt(:,1:Topt)))+...
                    sum(q'*Xopt(:,1:Topt))+sum(r'*Uopt);

tvec = 0:1:nsteps;
figure;
subplot(2,1,1);
set(gca,'FontSize',16);
stairs(tvec,[Xall(3,:),0],'k'); hold on;
stairs(tvec,[Xall(1,:),0],'k--'); 
title('mpchor5');
ylabel('xs');
subplot(2,1,2);
set(gca,'FontSize',16);
stairs(tvec,[Uall(4,:),0],'k'); hold on;
stairs(tvec,[Uall(3,:),0],'k--'); 
ylabel('us'); xlabel('t');
print('-depsc','supply_mpc.eps');

figure;
subplot(2,1,1);
set(gca,'FontSize',16);
stairs(tvec,Xopt(3,:),'k'); hold on;
stairs(tvec,Xopt(1,:),'k--'); 
title('opt');
subplot(2,1,2);
set(gca,'FontSize',16);
stairs(tvec,[Uopt(4,:),0],'k'); hold on;
stairs(tvec,[Uopt(3,:),0],'k--'); 
xlabel('t');
print('-depsc','supply_opt.eps');

save supply_data.mat
