% cholinc_pcg_test.m
% This MATLAB program demonstrates and evaluates
% the use of an incomplete Cholesky decomposition
% preconditioner in the conjugate gradient method.
% The code demonstrates the use of the method for
% a 3-D diffusion matrix on a grid of NxNxN points.
% K. Beers. MIT ChE. 10/1/03. v 6/14/05.
function iflag_main = cholinc_pcg_test(N,use_mod);
iflag_main = 0;

% First, set 3-D diffusion matrix
Ntot = N^3;  N_sq = N^2;
% set matrix elements
A = spalloc(Ntot,Ntot,7*Ntot);
for ix=1:N
    for iy=1:N
        for iz=1:N
            n = get_label_3D(ix,iy,iz,N,N);  % master label
            A(n,n) = 6;  % diagonal element
            if(iz > 1)  % U
                m = get_label_3D(ix,iy,iz-1,N,N);  A(n,m) = -1;
            end
            if(iz < N)  % L
                m = get_label_3D(ix,iy,iz+1,N,N);  A(n,m) = -1;
            end
            if(ix > 1)  % W
                m = get_label_3D(ix-1,iy,iz,N,N);  A(n,m) = -1;
            end
            if(ix < N)  % E
                m = get_label_3D(ix+1,iy,iz,N,N);  A(n,m) = -1;
            end
            if(iy > 1)  % S
                m = get_label_3D(ix,iy-1,iz,N,N);  A(n,m) = -1;
            end
            if(iy < N)  % N
                m = get_label_3D(ix,iy+1,iz,N,N);  A(n,m) = -1;
            end
        end
    end
end
disp('Finished setting A matrix');
nnz_A = nnz(A);  disp(['nnz(A) = ', int2str(nnz_A)]);


% solve system using pcg with no preconditioner
h_grid = (N+1)^(-2);  b = h_grid*ones(size(A,1),1);
% set options for pcg solver
tol = 1e-6; maxit = 2*Ntot;
[x,iflag,relres,iter_pcg_no_precond] = pcg(A,b,tol,maxit);

% Now, use incomplete Cholesky with varying
% values of the drop tolerance.
drop_tol_logmin = -6;  drop_tol_logmax = 0;
drop_tol_vect = logspace(drop_tol_logmin,drop_tol_logmax,10);
% store values of CPU time relative to use without
% preconditioner and numbers of non-zero elements
% in incomplete Cholesky factor
iter_pcg_cholinc = zeros(size(drop_tol_vect));
nnzR_cholinc = zeros(size(drop_tol_vect));
% set parameters for cholinc
if(use_mod)
    ICHOL_OPTS.michol = 1;
end
for k=1:length(drop_tol_vect)
    ICHOL_OPTS.droptol = drop_tol_vect(k);
    % generate incomplete Cholesky preconditioner
    R = cholinc(A,ICHOL_OPTS);  nnzR_cholinc(k) = nnz(R);
    [x,iflag,relres,iter_cholinc(k)] = pcg(A,b,tol,maxit,R',R);
end
% Next, plot performance
figure;  subplot(2,1,1);  semilogx(drop_tol_vect,iter_cholinc);
axis([ min(drop_tol_vect), max(drop_tol_vect), ...
        round(0.9*min(iter_cholinc)), round(1.1*max(iter_cholinc))]);
xlabel('drop tolerance in cholinc');  ylabel('# of pcg iterations');
title(['Effect of cholinc preconditioner on pcg, ', ...
        ' 3-D diffusion matrix, N = ', int2str(N)]);
gtext(['# CG iterations with no precond = ', int2str(iter_pcg_no_precond)]);
subplot(2,1,2);  loglog(drop_tol_vect,nnzR_cholinc,'-.');
% add upper bound and lower bound
hold on;  loglog(drop_tol_vect,nnz_A*ones(size(drop_tol_vect)),':');
drop_tol_logmid = (drop_tol_logmin+drop_tol_logmax)/2;
text(10^drop_tol_logmid,2*nnz_A,'nnz(A)');
nnz_Rup = 2*(N^5);
loglog(drop_tol_vect,nnz_Rup*ones(size(drop_tol_vect)),':');
text(10^drop_tol_logmid,2*nnz_Rup,...
    'upper bound on nnz(R), R=chol(A)');
axis([ min(drop_tol_vect), max(drop_tol_vect), ...
        0.1*nnz_A, 10*nnz_Rup]);
xlabel('drop tolerance in cholinc');  ylabel('nnz(R), R from cholinc');
% if use modified incomplete Cholesky, add note
if(use_mod)
    gtext('modified cholinc');
end

% make spy plots of A and of R for various values of the
% drop tolerance
figure;
% spy plot of A
subplot(2,2,1);  spy(A);
% spy plot of R with droptol = 1e-4;
ICHOL_OPTS.droptol = 1e-4;
R = cholinc(A,ICHOL_OPTS);
subplot(2,2,2);  spy(R);
gtext('droptol = 10^{-4}');
% spy plot of R with droptol = 1e-3;
ICHOL_OPTS.droptol = 1e-3;
R = cholinc(A,ICHOL_OPTS);
subplot(2,2,3);  spy(R);
gtext('droptol = 10^{-3}');
% spy plot of R with droptol = 1e-2;
ICHOL_OPTS.droptol = 1e-2;
R = cholinc(A,ICHOL_OPTS);
subplot(2,2,4);  spy(R);
gtext('droptol = 10^{-2}');

iflag_main = 1;
return;


% ----------------------------------------
% This routine computes the label of a grid
% point in the 3-D system.
function label = get_label_3D(ix,iy,iz,Nx,Ny);

label = (iz-1)*Nx*Ny + (ix-1)*Ny + iy;

return;
