%Matlab code for Question 7 of Chapter 11
%This program calculates posterior means and standard deviations for
%theta(1) and theta(2) using both Monte Carlo integration and Gibbs sampling
%norm-rnd is James LeSage's script for drawing from the multivariate Normal (0,SIGMA) 

%Specify the number of replications
r=100100;
%For Gibbs sampling, discard r0 burnin replications and specify starting value for theta(2)
r0=100;
th2draw=1;
%Specify the correlation between theta(1) and theta(2)
rho=0.999;
SIG= [ 1 rho; rho 1];

%Initialize MC and Gibbs sums to zero 
thgibbs=zeros(2,1);
thgibbs2=zeros(2,1);
thmc=zeros(2,1);
thmc2=zeros(2,1);
keepgraph=[];
for i = 1:r
    
   %Monte Carlo draw from bivariate Normal posterior
    thmcdraw=norm_rnd(SIG);
      
    %Gibbs sampling draw from 2 univariate Normal posterior conditionals
    th1mean= rho*th2draw;
    th1var = 1 - rho^2;
    th1draw = th1mean + norm_rnd(th1var);
    th2mean= rho*th1draw;
    th2var = 1 - rho^2;
    th2draw = th2mean + norm_rnd(th2var);
    if i>r0
        %For Gibbs discard r0 burnin draws
        %Don't need to do this for MC, but we do to maintain comparability with Gibbs results
        thmc=thmc+thmcdraw;  
        thmc2=thmc2+thmcdraw.^2;
        thdraw = [th1draw; th2draw];
        thgibbs=thgibbs+thdraw;  
        thgibbs2=thgibbs2+thdraw.^2; 
        keepgraph=[keepgraph; [i-r0 thmcdraw(1,1) th1draw]];
    end
end
'Number of burn in replications'
r0
r1=r-r0;
'Number of included replications'
r1
'Correlation between theta(1) and theta(2)'
rho
thmc=thmc./r1;
thmc2=thmc2./r1;
thmcsd=sqrt(thmc2 - thmc.^2);
'Monte Carlo Posterior Mean and Standard Deviation'
[thmc thmcsd]
thgibbs=thgibbs./r1;
thgibbs2=thgibbs2./r1;
thgibbssd=sqrt(thgibbs2 - thgibbs.^2);
'Gibbs Sampling Posterior Mean and Standard Deviation'
[thgibbs thgibbssd]

figure(1)
plot(keepgraph(:,1),keepgraph(:,2),'-', keepgraph(:,1),keepgraph(:,3),':')
legend('Monte Carlo','Gibbs sampler')
title('Figure 1: Draws of \theta_{1} for \rho = 0.999')
xlabel('Replication Number')
