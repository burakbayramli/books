%nm652.m 
% discretize a state equation x'(t)=Ax(t)+Bu(t) to x[n+1]=Ad*x[n]+Bd*u[n]
clear, clf
A=[0 1;0 -1]; B=[0;1];
x0=[1 -1]; t0=0; tf=2;
for itr=1:2
  T= 0.2; if itr==2, T=0.05; end
  eT= exp(-T);
  AD=[1 1-eT; 0 eT] %discretized system matrices obtained analytically
  BD=[T+eT-1; 1-eT]
  [Ad,Bd]= c2d_steq(A,B,T,100)
  [Ad1,Bd1]= c2d(A,B,T)
  t(1)=0;  xd(1,:)=x0;
  for n=1:(tf-t0)/T
     t(n+1)=n*T; xd(n+1,:)=xd(n,:)*Ad'+Bd';
  end
  if itr==1
    stairs([0; t'],[x0; xd],':')
    hold on, pause
   else
    stairs([0; t'],[x0; xd])
  end
end
N=100; t=t0+[0:N]'*(tf-t0)/N;
x(:,1)=t-1+2*exp(-t);
x(:,2)=1-2*exp(-t);
plot(t,x)
