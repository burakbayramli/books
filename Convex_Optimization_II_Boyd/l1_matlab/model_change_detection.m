% Model change detection example
% estimates changes in AR model
% y(t+2) = a(t)y(t+1)+b(t)y(t)+v(t), v(t):iid N(0,sigma^2)
%
% EE364b Convex Optimization II, S. Boyd
% Written by Seung-Jean Kim and Kwangmoo Koh 02/07/07

clear all; close all;
rand('state',0); randn('state',1);

T  = 300;   % total time
Ta = 100;   % change points of a
Tb = 200;   % change points of b

% generate true model
a  = [+1.0*ones(1,Ta) -0.5*ones(1,T-Ta-2)]';
b  = [-0.8*ones(1,Tb) -0.2*ones(1,T-Tb-2)]';

% generate time series
sigma = 0.5;
y = zeros(T,1);
for t = 1:T-2
    y(t+2) = a(t)*y(t+1)+b(t)*y(t)+sigma*randn(1);
end

% generate difference matrix
D = sparse(T-3,T-2,0);
for i = 1:T-3
    D(i,i) = -1; D(i,i+1) = 1;
end

% initialize parameters and weights
MAXITER = 5;
epsilon = 0.005;
lambda  = 1e-4;
gamma   = 10;
wa      = gamma*ones(T-3,1);
wb      = gamma*ones(T-3,1);

% solve the problem using iterated weighted l1 heuristic
for i = 1:MAXITER

    disp(sprintf('iterated weighted l1 heuristic iteration = %d',i));

    cvx_begin
    variables an(T-2) bn(T-2) resid(T-2);
    minimize( sum_square(resid)+...
              norm(wa.*(D*an),1)+norm(wb.*(D*bn),1)+...
              lambda*sum_square(an)+lambda*sum_square(bn) );
    subject to
        for t = 1:T-2
            resid(t)==y(t+2)-an(t)*y(t+1)-bn(t)*y(t);
        end
        bn<= 0;
        bn>=-1;
        an<= 2;
        an>=-2;
    cvx_end

    % store results
    as{i} = an;
    bs{i} = bn;

    % update weights
    wa = 1./(epsilon+abs(D*an)); 
    wb = 1./(epsilon+abs(D*bn)); 
end


% plot time series
figure(1);
set(gca,'FontSize',16);
plot(y); ylabel('y'); xlabel('t');
print -depsc model_change_detection_y.eps

% plot true model
figure(2);
set(gca,'FontSize',16);
x = [1:T-2]';
plot(x,a,'b-',x,b,'r-','LineWidth',2);
axis([1 T -1.2 1.2]); xlabel('t');
text(300,-0.5,'a'); text(300,-0.2,'b');
print -depsc model_change_detection_ab.eps

% plot reconstructed model using l1 heuristic
figure(3);
set(gca,'FontSize',16);
plot(x,a,'b-',x,b,'r-',x,as{1},'b-.',x,bs{1},'r-.','LineWidth',2);
axis([1 T -1.2 1.2]); xlabel('t');
print -depsc model_change_detection_l1.eps

% plot reconstructed model using iterated weighted l1 heuristic (5x)
figure(4);
set(gca,'FontSize',16);
plot(x,a,'b-',x,b,'r-',x,as{MAXITER},'b-.',x,bs{MAXITER},'r-.','LineWidth',2);
axis([1 T -1.2 1.2]); xlabel('t');
print -depsc model_change_detection_refinement.eps



