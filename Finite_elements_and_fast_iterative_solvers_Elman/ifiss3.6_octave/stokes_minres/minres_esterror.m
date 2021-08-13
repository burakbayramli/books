function [x,resvec,info]=minres_esterror(A,B,frhs,grhs,...
                    maxit,tol,sol,Q,domain,qmethod,premethod,eigmethod);
%MINRES_ESTERROR MINRES with error estimation:minimizes ||r_k||_inv(P) 
% [xmr,resvec,info]=minres_esterror(Ast,Bst',fst,gst,99,1e-6,xst,Q,domain,qmethod,1,1);
%   input
%         A,B          coefficient matrix [A,B;B',0]  
%         frhs,grhs    RHS vector [frhs;grhs]
%         maxit        max number of iterations
%         tol          stopping tolerance
%         sol          exact solution 
%         Q            Schur complement matrix: used to define E-norm 
%         domain       domain: used to define the infsup constant
%         qmethod      mixed method switch
%         premethod    1/0 exact/amg switch
%         eigmethod    1/0 infsup constant exact/estimate switch 
%   output
%         x            iterative solution
%         resvec       residual norm vector  
%         info         computed eigenvalue estimates 
% Current version: MINRES via Lanczos with P inner product
% calls functions:  hydro
%                   amg_grids_setup, amg_smoother_params,
%                   amg_smoother_setup, m_amgzz
%   IFISS function: DJS; 2 January 2011.
% Copyright (c) 2010 D.J. Silvester, V. Simoncini 
global amg_grid amg_smoother number_vcycles
rhs=[frhs;grhs];x0=zeros(size(rhs));
if premethod==1,
    fprintf('Ideal block preconditioning ..\n')
    fprintf('factorizing matrices ... ')
    [LA,UA]=lu(A); [LQ,UQ]=lu(Q);  % compute factors of matrices A and Q
    fprintf('done\n')
elseif premethod==0,
    fprintf('Inexact AMG block preconditioning ..\n')
    number_vcycles = default('number of V-Cycles? (default 1)',1);
amg_grid = amg_grids_setup(A);
aparams = struct('A',A);
fprintf('AMG with point damped Gauss-Seidel smoothing ..\n')
smoother_params = amg_smoother_params(amg_grid, 'PGS', 2);
amg_smoother = amg_smoother_setup(amg_grid, smoother_params);
[LQ,UQ]=lu(Q); % compute factors of the matrix  Q
else, error('Preconditioning choice is not included!')
end
%
% set up matrices
na=size(A,1); nb=size(B,2);
M=[A,B;B',sparse(nb,nb)];
E=[A,sparse(na,nb);sparse(nb,na),Q];
n=size(M,1);
%
if eigmethod==1,
%%% precompute the inf-sup constant
fprintf('Precomputing the infsup constant \n')
if exist('LA','var')==0,   [LA,UA]=lu(A); end
Q=0.5*(Q+Q'); ee = eigs(B'*(UA\(LA\B)),Q,2,'sm');
if domain==3, infsup=ee(1); else infsup=ee(2);  end
info=infsup;
else
fprintf('Dynamic estimation of the infsup constant\n')
infsup=[nan,nan,nan]; lambda=[nan,nan,nan];
end
b=rhs;
if domain~=3, x=hydro(x0,na,nb,qmethod);  sol=hydro(sol,na,nb,qmethod); end
x = x0;
res0=b-M*x;
res=zeros(n,1);
%
if premethod==1,
res(1:na)=UA\(LA\res0(1:na)); res(na+1:n)=UQ\(LQ\res0(na+1:n));
elseif premethod==0,
res(1:na)=m_amgzz(res0(1:na),aparams);
   if qmethod==3,
   fprintf('diagonal mass matrix preconditioning \n')
   qparams = struct('Q',Q,'qmethod',qmethod);
   res(na+1:n)=m_massdiag(res0(na+1:n),qparams);
   elseif qmethod==2
   fprintf('Chebyshev iteration preconditioning ..\n')
   number_its = default('number of iterations? (default 10)',10);
   qparams = struct('Q',Q,'its',number_its,'qmethod',-qmethod);
   res(na+1:n)=m_masscheb(res0(na+1:n),qparams);
   end
end

arr=sqrt( res0'*res);
erro=arr; 
resvec(1)=arr; 
err=sqrt(norm((sol-x)'*E*(sol-x)));

% Inizialize residual
   rr1=res0;

   R = zeros(4,1); 

% Q contains rotations. c_k, s_k could be stored instead
   QQ=speye(2);   

% Lanczos vectors
   v0=zeros(n,1); w0=zeros(n,1);

% Auxiliary vectors for updating solution
   p0=zeros(n,1); p=zeros(n,1);

   v = res/arr;
   w = res0/arr;  
   F=arr*speye(2,1); 
   i=0;
tic

   while (i<maxit & erro>tol)

     i=i+1; 
     i1=i+1; 
     QQ(i1,i1)=1;

% Matrix-vector product
     w1 = M*v;       

% Apply left precond P using P inner product for V.
    if premethod==1,
     v1(1:na,1)=UA\(LA\w1(1:na,1)); v1(na+1:n,1)=UQ\(LQ\w1(na+1:n,1));
    elseif premethod==0,
     v1(1:na,1)=m_amgzz(w1(1:na),aparams);
        if qmethod==3, 
        v1(na+1:n,1)=m_massdiag(w1(na+1:n),qparams);
        elseif qmethod==2
        v1(na+1:n,1)=m_masscheb(w1(na+1:n),qparams);
        end 
    end

    if (i>1)
         H(i-1,i)=v0'*w1;
         v1 = v1 - v0*H(i-1,i);
         w1 = w1 - w0*H(i-1,i);
    end
    H(i,i)=v'*w1;
    v1 = v1 - v*H(i,i);
    w1 = w1 - w*H(i,i);

    H(i+1,i)=sqrt(v1'*w1);
 %%
 if eigmethod ==0 & i>2;
 %%% estimate the inf-sup constant
 %%% compute ritz values
    eHh=sort(eig(H(1:i+1,1:i)'*H(1:i+1,1:i),H(1:i,1:i)'));
    ieHh=find(eHh<0); eHh_2infsup=max(eHh(ieHh));
    ieHh=find(eHh>0); delta=min(eHh(ieHh));
    theta2=1;
    emin=(eHh_2infsup.^2-eHh_2infsup*delta)/(delta*theta2);
    infsup=[infsup,emin]; lambda=[lambda,delta];
end
 

     if (H(i+1,i) ~= 0.) 
        v1=v1/H(i+1,i);     w1=w1/H(i+1,i); 
     else,return,end;
     

% Apply previous rotations
    if (i==1), R(3:4)=H(1:2,1); end
    if (i==2), R(2:4) = QQ(i-1:i1,i-1:i1)*H(1:i1,i);end
    if (i>2),  R(1:4) = QQ(i-2:i1,i-2:i1)*H(i-2:i1,i);end

% Determine and apply new rotations
    GG=give(R(3),H(i1,i));
    QQ(i:i1,1:i1)= GG *QQ(i:i1,1:i1);
    R(3:4)   = GG *R(3:4);
    F(1:2)     = GG *F(1:2);

% Update approx solution
    p1 = (v - p0*R(1)-p*R(2))/R(3);
    x  = x + p1*F(1);

    F(1:2)=[F(2);0];
    v0=v; v=v1;
    w0=w; w=w1;
    p0=p; p=p1;

    erro=full(abs(F(1)));  
    resvec=[resvec,erro];
    if domain~=3, x=hydro(x,na,nb,qmethod); end 
    err=[err,sqrt(norm((sol-x)'*E*(sol-x)))];
 %   fprintf('approx error: %g dim. %g \n',erro,i);

   end;

etoc=toc;
 
fprintf('convergence in %3i iterations\n',i)
nr0=resvec(1);
fprintf('\n    k  log10(||r_k||/||r_0||)   \n')
for its=1:i+1,
      fprintf('%5i %16.4f \n', its-1, log10(resvec(its)/nr0));
end
fprintf('Bingo!\n')
fprintf('\n  %9.4e seconds\n\n',etoc)  
if eigmethod ==0,
 fprintf('\n Eigenvalue convergence')
fprintf('\n    k     infsup     lambda   \n')
for its=4:i,
      fprintf('%5i %11.4f %11.4f \n', its, infsup(its), lambda(its));
end
info=[infsup(i),lambda(i)];  %% returns eigenvalue estimates via info
end
figure; %(13), subplot(122)
inx=0:i;rx=9:9:i+1;
%% fix legend
if eigmethod ==0, semilogy(inx(1),sqrt(2)*resvec(1)./infsup(1),'x-r'); 
else, semilogy(inx(1),sqrt(2)*resvec(1)/infsup,'x-r'); end
hold on
semilogy(inx,err,'-k');
semilogy(inx(1),resvec(1),'o-b');
%% plot bounds
if eigmethod ==0,
   semilogy(inx,sqrt(2)*resvec./infsup,'-r',rx-1,sqrt(2)*resvec(rx)./infsup(rx),'xr'); 
else, 
   semilogy(inx,sqrt(2)*resvec/infsup,'-r',rx-1,sqrt(2)*resvec(rx)/infsup,'xr');
end
semilogy(inx,resvec,'-b',rx-1,resvec(rx),'ob');
hold off
axis('square')
xlabel('iteration number {\it k}')
%ylabel('convergence history')
%title('nv=2')
if eigmethod ==0,
h=legend('${\sqrt{2}/ \gamma_k^2}\> ||r_k||_{M_*}$', ...
          '$||e_k||_E$','$||r_k||_{M_*}$');
else
h=legend('${\sqrt{2}/ \gamma^2}\> ||r_k||_{M_*}$', ...
          '$||e_k||_E$','$||r_k||_{M_*}$');
end
set(h,'Interpreter','latex')
