%empcdfr.m
clear all,clc
rand('state',123);
N = 50; %sample size
x = sort(-log(rand(1,N))); %generate and sort sample
x=[0,x]; % append 0 for plotting purposes
z =(0:N)/N;
zl = z - 1.65*sqrt(z.*(1-z)/N); % lower curve
zu = z + 1.65*sqrt(z.*(1-z)/N); % upper curve
axes('FontSize',16),hold on
for i=1:N %plot the confidence bounds
    line([x(i),x(i+1)],[zl(i),zl(i)],'LineWidth',3);
    line([x(i+1),x(i+1)],[zl(i),zl(i+1)]);
    line([x(i),x(i+1)],[zu(i),zu(i)],'LineWidth',3);
    line([x(i+1),x(i+1)],[zu(i),zu(i+1)]);
end
t = 0:0.01:max(x);plot(t,1-exp(-t))

