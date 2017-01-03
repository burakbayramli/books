%nm651_2.m to solve a state equation x'(t)=Ax(t)+Bu(t)
clear
%Numerical Solution of Differential Equation
df= 'df651';  
t0=0; tf=2; x0=[1 -1]; N=45;
[t1,xH]=ode_Ham(df,[t0 tf],x0,N);
[t45,x45]=ode45(df,[t0 tf],x0);

disp('Solution of Differential Eq based on Laplace transform')
syms s t
A=[0 1;0 -1]; B=[0 1]'; x0=[1 -1]';
Xs=(s*eye(size(A))-A)^-1*(x0+B/s)
for n=1:size(A,1)
   xt(n)=ilaplace(Xs(n));
end
xt=transpose(xt)
t=t0+[0:N]'*(tf-t0)/N;
%xtt=[eval(xt(1)) eval(xt(2))];
plot(t1,xH), hold on
xtt=eval([xt(1) xt(2)]);
plot(t,xtt)

disp('Analytical solution')
[xt1,xt2]= dsolve('Dx1=x2, Dx2=-x2+1', 'x1(0)=1,x2(0)=-1')
