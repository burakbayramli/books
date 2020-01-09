% resistive network L*v = i
clear all; rand('state', 364);

n = 100000; %number of nodes
deg = 20; %on average a node is connected to deg other nodes
density = deg/n; 

L = -abs(sprandsym(n,density)); %conductance uniform [0,1]
v = L*ones(n,1);
Sdiagonal = spdiags(v,0,n,n);
L = L - Sdiagonal;

L(n,:) = []; L(:,n) = [];
c = rand(n-1,1);

M = [];
fprintf('\nStarting CG ...\n');
time_start = cputime;
[x,flag,relres,iter,resvec] = pcg(L,c, [], 0.01*n, M);
time_end = cputime;
fprintf('CG done. Status: %d\nTime taken: %d\n', flag, time_end - time_start); 
norm_res = norm(c-L*x);
semilogy(resvec/norm(c), '.--'); hold on;
set(gca,'FontSize', 16, 'FontName', 'Times');
xlabel('cgiter'); ylabel('relres');

Ldiag = diag(L);
M = spdiags(Ldiag,0,n-1,n-1);
fprintf('\nStarting CG with preconditioning...\n');
time_start = cputime;
[x,flag,relres,iter,resvec] = pcg(L,c, [], 0.01*n, M);
time_end = cputime;
fprintf('PCG done. Status: %d\nTime taken: %d\n', flag, time_end - time_start); 
semilogy(resvec/norm(c), 'k.-'); hold off;
print('-depsc', 'resnet_pcg_relres.eps');






