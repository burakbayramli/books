clear all,clc
N=10^6;
x=nan(N,1); alpha=10.5;
tic
for i=1:N
   %x(i)=aherns_gam_2(alpha);
   %x(i)=cheng_feast(alpha);
    x(i)=gamrand(alpha,3);
end

toc




%x=-log(rand(N,1)).*betarnd(alpha,1-alpha,[N,1]);
mean(x<5.5), gamcdf(5.5,alpha,1/3)

%ecdf(x), hold on

 %plot(0:0.01:max(x),gamcdf(0:0.01:max(x),alpha,1/3),'r')
 
 kde(x,2^14,[-0.001,max(x)]),hold all
 plot(0:0.001:max(x),gampdf(0:0.001:max(x),alpha,1/3),'r')
 
 
 