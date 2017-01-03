%nm6p10a
clear, clf
K=1.9e-9; Ta=400; Ta4=Ta^4;
df=inline('[T(2); 1.9e-9*(T(1).^4-256e8)]','t','T');
x0=0; xf=4; T0=500; Tf=300; N=500; tol=1e-5; kmax=10;
% Shooting method
[xx,T_sh]=bvp2_shoot(df,x0,xf,T0,Tf,N,tol,kmax);
% Iterative finite difference method
a1=0; a0=0;  u=T0+[1:N-1]*(Tf-T0)/N; 
for i=1:100
  [xx,T_fd]=bvp2_fdf(a1,a0,u,x0,xf,T0,Tf,N);
  u=K*(T_fd(2:N).^4-Ta4); %RHS of (P6.10-1)
  if i>1&norm(T_fd-T_fd0)/norm(T_fd0)<tol, i, break; end
  T_fd0=T_fd;
end
% MATLAB built-in function bvp4c
solinit = bvpinit(linspace(x0,xf,5),[Tf T0]);
fbc=inline('[Ta(1)-500; Tb(1)-300]','Ta','Tb');
tic, sol=bvp4c(df,fbc,solinit,bvpset('RelTol',1e-6)); 
T_bvp = deval(sol,xx); time_bvp=toc;
% The set of three solutions  
Ts=[T_sh(:,1) T_fd T_bvp(1,:)']; 
% Evaluates the errors and plot the graphs of the solutions
err=err_of_sol_de(df,xx,ys)
subplot(321), plot(xx,Ts)
