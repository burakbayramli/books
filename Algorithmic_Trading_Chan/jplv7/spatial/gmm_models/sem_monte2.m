% PURPOSE: A DEMO FILE CONTAINS A MONTE CARLO SIMULATION for sem2_gmm

%THE SIMULATION ALSO USES THE FUNCTION 'Pdweight' WHICH
%CREATES ROW-STANDARDIZED SPARSE WEIGHT MATRICES;
%SUCH THAT distance for neighborhood = (lower < distance <= upper);

clear all;

%Import data set (user must enter relevant path);
t=importdata('CenBlks.txt');

%Pull 1000 obs from the data set;
xcoord=t.data(1:1000,1);
ycoord=t.data(1:1000,2);

%PARAMETERS FOR WEIGHT MATRIX #1;
%Set lower and upper neighboorhood bounds for the spatial weight matrix #1;
lower=0;
upper=300;
%Set row standardization option to 1=yes;
RowStdOpt=1;
%Use 'Pdweight' to create row-standardized weight matrix;
W1 = pdweight(xcoord,ycoord,lower,upper,RowStdOpt);

%PARAMETERS FOR WEIGHT MATRIX #2;
%Set lower and upper neighboorhood bounds for the spatial weight matrix #1;
lower=300;
upper=600;
%Set row standardization option to 1=yes;
RowStdOpt=1;
%Use 'Pdweight' to create row-standardized weight matrix;
W2 = pdweight(xcoord,ycoord,lower,upper,RowStdOpt);


%%%%%%%BEGIN MONTE CARLO SIMULATION%%%%%%%%;
%Set inital parameters
TrueLambda1=.3;
TrueLambda2=.6;
beta(1,1) = -10;%intercept coefficient
beta(2,1) = 0.5;
beta(3,1) = 35;
sige1 = 1;
clear OUTPUT2;
i=1;

%User can enter the number of runs for the Monte Carlo simulation;
numsims=25;
for i=1:numsims;
    n = size(W1,1);
    k = 3;
    x = randn(n,k);
    x(:,1) = ones(n,1);
    y = x*beta + (speye(n) - TrueLambda1*W1 - TrueLambda2*W2)\(randn(n,1)*sqrt(sige1)); 

    %Call sem2_gmm
    test1=sem2_gmm(y,x,W1,W2);

    %Results from SEMGM2 are placed in a matrix called OUTPUT2;
    OUTPUT2(i,1)=test1.lambda(1);OUTPUT2(i,2)=test1.lambda(2);OUTPUT2(i,3)=test1.GMsige(1);
    OUTPUT2(i,4)=test1.beta(1);OUTPUT2(i,5)=test1.beta(2);OUTPUT2(i,6)=test1.beta(3);
    i=i+1;
 end;

%%%%%%%%%%%%%%%%%%%%%%
%User can get an idea of the distribution of the parameter estimates by...;

%Print Results (of last run);
disp('printout of estimation results from last run');
prt(test1)


truth = [TrueLambda1 TrueLambda2 sige1 beta'];
%Simple summary stats;
mout = mean(OUTPUT2);
sout = std(OUTPUT2);

out = [truth
       mout
       sout];

in.cnames = strvcat('lambda1','lambda2','sigma','beta0','beta1','beta2');
in.rnames = strvcat('Statistics','true value','mean','std deviation');

mprint(out,in);


%Plotting the results in a histogram
%%%%%%%%%%%%HISTOGRAM FOR W #1  %%%%%%%%%%%%%%%%%%%%%%5
hist(OUTPUT2(:,1),numsims);
h = findobj(gca,'Type','patch');
set(h,'FaceColor','r','EdgeColor','w');
hold on;
title(sprintf('Histogram of Lambda #1 values for N = %4.0f',numsims));
xlabel('Lambda');ylabel('Num. Obs.');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%HISTOGRAM FOR W #2  %%%%%%%%%%%%%%%%%%%%%%5
hist(OUTPUT2(:,2),numsims);
h = findobj(gca,'Type','patch');
set(h,'FaceColor','r','EdgeColor','w');
hold on;
title(sprintf('Histogram of Lambda #2 values for N = %4.0f',numsims));
xlabel('Lambda');ylabel('Num. Obs.');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%