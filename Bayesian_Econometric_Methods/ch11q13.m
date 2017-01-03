%Matlab code for Question 13 of Chapter 11
%This program calculates posterior means and standard deviations for
%theta using both independence and random walk chain Metropolis-Hastings algorithms 

%Specify the number of replications
r=10100;
%Discard r0 burnin replications and specify starting value for theta
r0=100;
thdrawic=1;
thdrawrw=thdrawic;
%rwsd is the standard deviation for the increment in the RW M-H algorithm
rwsd=4;
icsd=6;
%Initialize MH sums to zero 
thmean=zeros(2,1);
th2mo=zeros(2,1);
%count the number of accepted draws
countic=0;
countrw=0;
for i = 1:r
    %Draw a candidate for independence chain
    thcanic = icsd*randn;
    %Calculate acceptance probability
    accic = min(exp(.5*(abs(thdrawic) - abs(thcanic) + (thcanic/icsd)^2 - (thdrawic/icsd)^2)),1);
    
    %accept candidate draw with probability accic
    if accic > rand
        thdrawic=thcanic;
        countic=countic+1;
    end
     %Draw a candidate for independence chain
    thcanrw = thdrawrw + rwsd*randn;
    %Calculate acceptance probability
    accrw = min(exp(.5*(abs(thdrawrw) - abs(thcanrw))),1);
   
    %accept candidate draw with probability accrw
    if accrw > rand
        thdrawrw=thcanrw;
        countrw=countrw+1;
    end
   
    if i>r0
        %discard r0 burnin draws
        thdraw = [thdrawic; thdrawrw];
        thmean=thmean+thdraw;  
        th2mo=th2mo+thdraw.^2;
    end
end
'Number of burn in replications'
r0
r1=r-r0;
'Number of included replications'
r1
'Proportion of accepted candidate draws: Independence chain M-H'
countic/r
'Proportion of accepted candidate draws: Random walk chain M-H'
countrw/r
thmean=thmean./r1;
th2mo=th2mo./r1;
thvar=th2mo - thmean.^2;
'Posterior Mean and Variance, Ind Chain then RW Chain'
[thmean thvar]
