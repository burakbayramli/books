% Solves l2-regularized logistic regression problem
%   minimize (1/m) sum_i log(1+exp(-b_i(x_i*w))) + sum_j lambda_i*w_j^2
% using truncated Newton method.
%
% EE364b Convex Optimization II, S. Boyd
% Written by Kwangmoo Koh, 02/20/07
% 
fprintf(1,'\nThis script takes a few minutes to run...\n\n');

%--------------------------------------------------------------------
%   Generate/Load problem instance
%--------------------------------------------------------------------

n = 10000; m = 2*n; m0 = m/2; m1 = m/2;

offset = 1;
nnz_per_example = 5;

file_name = sprintf('problem_%dx%d.mat',m,n);
if ~exist(file_name)
    rand('state',1); % set state so that problem is reproducible
    randn('state',1);
    % ensure that all columns/rows become non-trivial
    Z  = sparse(1:m,mod(randperm(m),n)+1,randn(m,1));
    X1 = sprandn(m1,n,(nnz_per_example-1)/n)+Z(1:m1,:);
    X0 = sprandn(m0,n,(nnz_per_example-1)/n)+Z(m1+1:end,:);
    b  = [ones(m1,1) ;-ones(m0,1)];
    X  = [X1+offset*spones(X1); X0-offset*spones(X0)];
    X  = ceil(X*100)/100; % truncate below hundredths
    clear X1 X0 Z;
    save(file_name,'X','b','n','m','-V4');
else
    load(file_name);
    [m,n] = size(X);
end

lambda = 1e-8;

time = zeros(7,1);

%--------------------------------------------------------------------
%   Solve the problem using CG with various parameters
%--------------------------------------------------------------------
tic; [w,s,history{1}] = l2_logreg(X,b,lambda,'cg', 1e-4, 10);  time(1) = toc
tic; [w,s,history{2}] = l2_logreg(X,b,lambda,'cg', 1e-4, 50);  time(2) = toc
tic; [w,s,history{3}] = l2_logreg(X,b,lambda,'cg', 1e-4, 250); time(3) = toc

%--------------------------------------------------------------------
%   Solve the problem using exact method
%--------------------------------------------------------------------
tic; [w,s,history{4}] = l2_logreg(X,b,lambda,'exact', [], []); time(4) = toc

%--------------------------------------------------------------------
%   Solve the problem using PCG with various parameters
%--------------------------------------------------------------------
tic; [w,s,history{5}] = l2_logreg(X,b,lambda,'pcg', 1e-4, 10); time(5) = toc
tic; [w,s,history{6}] = l2_logreg(X,b,lambda,'pcg', 1e-4, 50); time(6) = toc
tic; [w,s,history{7}] = l2_logreg(X,b,lambda,'pcg', 1e-4, 250);time(7) = toc

%--------------------------------------------------------------------
%   Plots
%--------------------------------------------------------------------

% norm(gradient) vs TN iterations for CG, exact method
figure(1)
cols1 = {'bo-','rd-','k^-','bs-','mv-','c+-'};
set(gca,'FontSize',16);
for i = [1 2 3 4]
    semilogy([0:size(history{i},2)-1], history{i}(2,:),cols1{i},'LineWidth',1.5);

    hold on;
end
hold off;
axis([0 30 1e-8 1e-1]);
xlabel('iter'); ylabel('normg');
legend('cgnmax10','cgnmax50','cgnmax250','exact');
print -depsc tnm_normg_vs_tntiter.eps

% norm(gradient) vs CG iterations for CG, exact method
figure(2)
cols2 = {'b-','r-','k-','b-','m-','c-'};
set(gca,'FontSize',16);
for i = [1 2 3]
    
    X1 = []; X2 = []; maxidx = size(history{i},2);
    X1(1:2:maxidx*2-1) = history{i}(3,1:maxidx);
    X1(2:2:maxidx*2-2) = history{i}(3,2:maxidx);

    X2(1:2:maxidx*2-1) = history{i}(2,1:maxidx);
    X2(2:2:maxidx*2-2) = history{i}(2,1:maxidx-1);
    
    semilogy(X1,X2,cols2{i},'LineWidth',1.5);
    hold on;
end
hold off;
axis([0 1800 1e-8 1e-1]);
xlabel('cgiter'); ylabel('normg');
legend('cgnmax10','cgnmax50','cgnmax250');
print -depsc tnm_normg_vs_cgiter.eps

% norm(gradient) vs CG iterations for CG, PCG
figure(3)
cols3 = {'b:','r:','k:','b-','b-','r-','k-'};
set(gca,'FontSize',16);
for i = [1 2 3 5 6 7]
    
    X1 = []; X2 = []; maxidx = size(history{i},2);
    X1(1:2:maxidx*2-1) = history{i}(3,1:maxidx);
    X1(2:2:maxidx*2-2) = history{i}(3,2:maxidx);

    X2(1:2:maxidx*2-1) = history{i}(2,1:maxidx);
    X2(2:2:maxidx*2-2) = history{i}(2,1:maxidx-1);
    
    semilogy(X1,X2,cols3{i},'LineWidth',1.5);
    hold on;
end
hold off;
axis([0 1800 1e-8 1e-1]);
xlabel('pcgiter'); ylabel('normg');
legend('cgnmax10','cgnmax50','cgnmax250','pcgnmax10','pcgnmax50','pcgnmax250');
print -depsc tnm_normg_vs_pcgiter.eps
