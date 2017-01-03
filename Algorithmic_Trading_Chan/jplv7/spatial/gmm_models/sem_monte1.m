% PURPOSE: A DEMO FILE CONTAINS A MONTE CARLO SIMULATION for sem_gmm
%THE SIMULATION ALSO USES THE FUNCTION 'Pdweight' WHICH
%CREATES ROW-STANDARDIZED SPARSE WEIGHT MATRICES;
%SUCH THAT distance for neighborhood = (lower < distance <= upper);

clear all;
%Import data set (user must enter relevant path);
t=importdata('CenBlks.txt');

%Pull 1000 obs from the data set;
xcoord=t.data(1:1000,1);
ycoord=t.data(1:1000,2);

%Set lower and upper neighboorhood bounds for the spatial weight matrix;
lower=0;
upper=400;

%Set row standardization option to 1=yes;
RowStdOpt=1;
%Use 'Pdweight' to create row-standardized weight matrix;
W1 = pdweight(xcoord,ycoord,lower,upper,RowStdOpt);


%%%%%%%BEGIN MONTE CARLO SIMULATION%%%%%%%%;
%Set inital parameters
TrueLambda1=.9;
beta(1,1) = -1;%intercept coefficient
beta(2,1) = 0.5;
beta(3,1) = 3;
sige1 = 10;
clear OUTPUT1;
i=1;

%User can enter the number of runs for the Monte Carlo simulation;
numsims=10;
for i=1:numsims;
    n = size(W1,1);
    k = 3;
    x = randn(n,k);
    x(:,1) = ones(n,1);
    y = x*beta + (speye(n) - TrueLambda1*W1)\(randn(n,1)*sqrt(sige1)); 

    %Call sem_gmm
    test1=sem_gmm(y,x,W1);

    %Results from sem_gmm are placed in a matrix called OUTPUT1;
    OUTPUT1(i,1)=test1.lambda;OUTPUT1(i,2)=test1.GMsige(1);OUTPUT1(i,3)=test1.beta(1);
    OUTPUT1(i,4)=test1.beta(2);OUTPUT1(i,5)=test1.beta(3);
    i=i+1;
end;

%%%%%%%%%%%%%%%%%%%%%%
%Print Results (of last run);
disp('estimation results from last run');
prt(test1)

%User can get an idea of the distribution of the parameter estimates by...;

truth = [TrueLambda1 sige1 beta'];
%Simple summary stats;
mout = mean(OUTPUT1);
sout = std(OUTPUT1);

out = [truth
       mout
       sout];

in.cnames = strvcat('lambda','sigma','beta0','beta1','beta2');
in.rnames = strvcat('Statistics','true value','mean','std deviation');

mprint(out,in);

%Plotting the results in a histogram
%%%%%%%%%%%%HISTOGRAM FOR 1 W%%%%%%%%%%%%%%%%%%%%%%5
hist(OUTPUT1(:,1),numsims);
h = findobj(gca,'Type','patch');
set(h,'FaceColor','r','EdgeColor','w');
hold on;
title(sprintf('Histogram of Lambda values for N = %4.0f',numsims));
xlabel('Lambda');ylabel('Num. Obs.');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
