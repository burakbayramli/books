%cd_gmres_test   test preconditioned GMRES for convection-diffusion eqn
%   IFISS scriptfile: HCE; 28 January 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

afun_par = struct('Afun','a_cdt','A',Asupg);
x0 = zeros(length(fsupg),1);
tol = 1e-6;
maxit = 100;
params = [tol,maxit,1];

% Four-directional Gauss-Seidel preconditioning
n = sqrt(size(Asupg,1));

Q1 = tril(Asupg,1);
Q2 = diag(diag(Asupg,0)) + diag(diag(Asupg,-n),-n) + diag(diag(Asupg,n),n) ...
   + diag(diag(Asupg,-(n+1)),-(n+1)) + diag( diag(Asupg,-1),-1)        ...
   + diag(diag(Asupg,(n-1)),(n-1));
Q3 = triu(Asupg,-1);
Q4 = diag(diag(Asupg,0)) + diag(diag(Asupg,-n),-n) + diag(diag(Asupg,n),n) ...
   + diag(diag(Asupg,(n+1)),(n+1)) + diag( diag(Asupg,1),1)            ...
   + diag(diag(Asupg,-(n-1)),-(n-1));
n = sqrt(size(Asupg,1));

[L1,U1] = lu(Q1);
[L2,U2] = lu(Q2);
[L3,U3] = lu(Q3);
[L4,U4] = lu(Q4);

mfun_par=struct('Mfun','m_gs_four',...
                 'L1',L1,'U1',U1,'L2',L2,'U2',U2, ...
                 'L3',L3,'U3',U3,'L4',L4,'U4',U4);
[x_gs4,flag,iter_gs4,resvec_gs4] = gmres_r(afun_par,mfun_par,fsupg,params,x0);


% ILU(0) preconditioning
[L,U]=luinc(Asupg,'0'); 
mfun_par=struct('Mfun','m_ilut','L',L,'U',U);
[x_ilu,flag,iter_ilu,resvec_ilu] = gmres_r(afun_par,mfun_par,fsupg,params,x0);


% "Two-directional" ILU(0) preconditioning
[Aver,p,ip] = hor_to_ver(Asupg);
[Lv,Uv] = luinc(Aver,'0');
mfun_par=struct('Mfun','m_ilut_2dir',...
                   'L',L,'U',U, 'Lv',Lv,'Uv',Uv, 'p',p,'ip',ip);
[x_ilu2,flag,iter_ilu2,resvec_ilu2] = gmres_r(afun_par,mfun_par,fsupg,params,x0);