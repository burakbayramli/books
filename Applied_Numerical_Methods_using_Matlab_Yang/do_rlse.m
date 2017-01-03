%do_rlse.m
clear
xo=[2  1]'; %The true value of unknown coefficient vector
xo=[1  2]'; %The true value of unknown coefficient vector
for k=1: 100
   A(k,1) =0.01*k;  A(k,2) =1;
   B(k,1) =A(k,:)*xo +0.2*rand(1,1);
end
x =rlse(A,B) %Program 2.2
x1 =lin_eq(A,B) %Program 2.1
%x1 =(A'*A)^-1*A'*B %To compare with Eq.(2.11)
clf
plot(A(:,1),B(:,1),'b+',A(:,1),A*x,'r')

clear('A')
NA=length(xo); 
xk_on =zeros(NA,1);
P =10000*eye(NA,NA);
%P =0.01*eye(NA,NA);
time_on=0; time_off=0;
for k=1:100
   A(k,:)=[k*0.01 1];
   b(k,:)=A(k,:)*xo+0.2*rand;
   tic, [xk_on,K,P] =rlse_online(A(k,:),b(k,:),xk_on,P); time_on=time_on+toc;
   tic, xk_off=A\b; time_off=time_off+toc;
end
norm(xk_on-xk_off)