% randsys_mpc.m
% compares the performance of finite horizon approximation
% and model predictive control on a randomly generated system
% EE364b, Convex Optimization II, S. Boyd, Stanford University.
% Written by Yang Wang, 04/2008

% generate A, B matrices
randn('state',0); rand('state',0);
n = 3; m = 2;
A = randn(n,n); B = rand(n,m);
[U,S,V] = svd(A); A = U*V;

% objective and constraints
N = 50; Q = eye(n); R = eye(m);
Qhalf = sqrtm(Q); Rhalf = sqrtm(R);
xmax = 1*ones(n,1); xmin = -1*ones(n,1);
umax = 0.5*ones(m,1); umin = -0.5*ones(m,1);
z = [-0.9;0.9;-0.9]; optvalfha = zeros(N,1);
Xallfha = zeros(n,N,N); Uallfha = zeros(m,N,N);

% optimal value
Topt = 200;
cvx_begin
variables Xopt(n,Topt+1) Uopt(m,Topt)
    max(Xopt') <= xmax'; max(Uopt') <= umax';
    min(Xopt') >= xmin'; min(Uopt') >= umin';
    Xopt(:,2:Topt+1) == A*Xopt(:,1:Topt)+B*Uopt;
    Xopt(:,1) == z; 
    minimize (norm([Qhalf*Xopt(:,1:Topt); Rhalf*Uopt],'fro'))
cvx_end
Jopt = cvx_optval.^2;

% finite horizon approximation
for T = 1:N
    cvx_begin
        variables X(n,T+1) U(m,T)
        max(X') <= xmax'; max(U') <= umax';
        min(X') >= xmin'; min(U') >= umin';
        X(:,2:T+1) == A*X(:,1:T)+B*U;
        X(:,1) == z; X(:,T+1) == 0;
        minimize (norm([Qhalf*X(:,1:T); Rhalf*U],'fro'))
    cvx_end
    optvalfha(T) = cvx_optval;
    Xallfha(:,1:T,T) = X(:,1:T);
    Uallfha(:,1:T,T) = U;
end
optvalfha = optvalfha.^2;

set(gca,'Fontsize',16);
plot(optvalfha,'k'); xlabel('T'); ylabel('optvalfha');
hold on; plot(Jopt*ones(N,1),'k--');
print('-depsc','randsys_fha.eps');

tvec = 0:1:50;
figure;
subplot(2,1,1);
set(gca,'Fontsize',16);
stairs(tvec,[Xopt(1,1:50),0],'k');
axis([0,50,-1.1,1.1]); 
title('opt');
subplot(2,1,2); 
set(gca,'Fontsize',16);
stairs(tvec,[Uopt(1,1:50),0],'k');
axis([0,50,-0.6,0.6]); xlabel('t');
print('-depsc','optcontr.eps');

figure;
subplot(2,1,1); 
set(gca,'Fontsize',16);
stairs(tvec,[Xallfha(1,1:50,10),0],'k');
axis([0,50,-1.1,1.1]); ylabel('x1');
title('hor10');
subplot(2,1,2); 
set(gca,'Fontsize',16);
stairs(tvec,[Uallfha(1,1:50,10),0],'k');
axis([0,50,-0.6,0.6]); xlabel('t'); ylabel('u1');
print('-depsc','fhahor10.eps');

% model predictive control
minT = min(find(optvalfha ~= Inf));
optvalmpc = zeros(N,1); optvalmpc(1:minT-1) = Inf;
Xallmpc = zeros(n,N,N); Uallmpc = zeros(m,N,N);
for T = minT:N
    x = z;
    for i = 1:N
        cvx_precision(max(min(abs(x))/10,1e-6))
        cvx_begin 
            variables X(n,T+1) U(m,T)
            max(X') <= xmax'; max(U') <= umax';
            min(X') >= xmin'; min(U') >= umin';
            X(:,2:T+1) == A*X(:,1:T)+B*U;
            X(:,1) == x; X(:,T+1) == 0;
            minimize (norm([Qhalf*X(:,1:T); Rhalf*U],'fro'))
        cvx_end
        Xallmpc(:,i,T) = x; 
        u = U(:,1); Uallmpc(:,i,T) = u;
        optvalmpc(T) = optvalmpc(T)+x'*Q*x+u'*R*u;
        x = A*x+B*u;
    end
end

figure;
set(gca,'Fontsize',16);
plot(optvalfha,'k--'); hold on; plot(optvalmpc,'k'); 
plot(Jopt*ones(N,1),'k:');
xlabel('T'); ylabel('optvals');
print('-depsc','randsys_mpc.eps');

figure;
subplot(2,1,1);
set(gca,'Fontsize',16);
stairs(tvec,[Xallmpc(1,1:50,10),0],'k');
axis([0,50,-1.1,1.1]); ylabel('x1');
title('mpc');
subplot(2,1,2); 
set(gca,'Fontsize',16);
stairs(tvec,[Uallmpc(1,1:50,10),0],'k');
axis([0,50,-0.6,0.6]); xlabel('t'); ylabel('u1');
print('-depsc','mpchor10.eps');

save randsysresults.mat
