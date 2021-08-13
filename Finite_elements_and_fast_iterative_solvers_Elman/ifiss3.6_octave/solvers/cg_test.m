%CG_TEST CG convergence demo 
%   IFISS scriptfile: AR; 30 March 2005. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

% matrix dimension
N=100;
% select problem
matno=default('Test matrix? 1/2/3/4 (default 1)',1);
if matno==1
   fprintf('Matrix 1: evenly distributed eigenvalues\n')
   vec=[1:N];
elseif matno==2
   fprintf('Matrix 2: perfectly distributed eigenvalues\n')
   % zeros of Chebyshev ploynomial
   k=[0:N-1]; tvec=[cos(((2*k+1)*pi)/(2*N))];tvec=fliplr(tvec);
   vec=0.5*(N-1)*tvec+0.5*(N+1);
   % move end-points to get condition number of N
   vec(1)=1; vec(N)=N;
elseif matno==3
   fprintf('Matrix 3: five clusters of eigenvalues\n')
   svec=ones(1,N/5); spr=1e-1;spr2=spr/2;
   vec=[(1+spr*rand(size(svec))) (0.25*N-spr2+spr*rand(size(svec))) ...
    (0.5*N-spr2+spr*rand(size(svec))) (0.75*N-spr2+spr*rand(size(svec))) ...
    (N-spr*rand(size(svec)))];
   % move end-points to get condition number of N
   vec(1)=1; vec(N)=N;
elseif matno==4
   fprintf('Matrix 4: two distinct eigenvalues\n')
   vec=[ones(1,N/2) N*ones(1,N/2)];
else
   error('Invalid test problem!')
end

% set up diagonal test matrix
W=diag(vec);

% RHS vector
rhs=ones(N,1);

% initial guess
x0=zeros(N,1);

x_exact=W\rhs;

% solve via CG
[x_it,flag,relres,iter,resvec]=pcg(W,rhs,1e-6,110);

% monitor convergence
if flag ==0
   % successful convergence
   fprintf('Convergence in %3i CG iterations\n',iter)
%  nr0=resvec(1);
%  fprintf('\n    k  log10(||r_k||/||r_0||)   \n')
%  for its=1:iter+1
%     fprintf('%5i %16.4f \n', its-1, log10(resvec(its)/nr0));
%  end
%  fprintf('Bingo!\n\n')
   %%% plot residuals
   resplot(resvec)
else
   fprintf('Iteration aborted! Iteration returned with flag equal to %2i \n',flag)
   resplot(resvec)
end
