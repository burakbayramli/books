%program which makes the graphs for chapter 18 Exercise 2
%Based on Sims and Uhlig (1991, Econometrica)

%number of artificial data sets for each value of theta
nrep=10000;
%size of artificial data sets
t=100;
%number of values for theta
nthet=31;
%the values of theta
thetgrid=zeros(nthet,1);
thetgrid(1,1)=.8;
for i=2:nthet
    thetgrid(i,1)=thetgrid(i-1,1)+.01;
end

%the upper bounds of each bin for making a histogram
binbound=zeros(nthet+2,1);
binbound(1,1)=.795;
for i=2:nthet+1
    binbound(i,1)=binbound(i-1,1)+.01;
end
binbound(nthet+2,1)=1e200;


%initialize the matrix which stores the frequency theta-hat lies in each bin
freqs = zeros(nthet+2,nthet);

for ithet = 1:nthet
    for irep = 1:nrep
        %simulate an artificial data set
             y=zeros(t,1);
             y(1,1)=randn;
             for it = 2:t
                 y(it,1)= thetgrid(ithet,1)*y(it-1,1) + randn;
             end
         %now get MLE (remember that we are setting y0=0
         thethat = (y(2:t,1)'*y(1:t-1,1))/ (y(1:t-1,1)'*y(1:t-1,1));
         
         %now add to appropriate bin in histogram
         for i=1:nthet+2
             if thethat < binbound(i,1)
                 freqs(i,ithet) = freqs(i,ithet)+1;
                 break
             end
         end
         
         
         end
     end

     
     %turn the frequencies into proportions to make the resulting graph look like a discrete dist
     props=freqs./nrep;
     thet1=[.79; thetgrid; 1.11];
     %Now make a 3D graph using all the histograms
     figure(1)
     mesh(thetgrid,thet1,props)
      axis([.79,1.11,.79,1.11,0,1])
      title('Figure 18.1: Joint Histogram involving \theta and MLE')
      xlabel('MLE')
      ylabel('\theta')
       zlabel('Proportion')
      %now make a 2D figure which is a slice right at theta=1
      figure(2)
      props1=props(:,21);
      bar(thet1,props1)
       title('Figure 18.2: Sampling distribution when \theta=1')
      xlabel('MLE')
      ylabel('Proportion')
     
      %now make a 2D figure which is a slice right at theta-hat=1
      figure(3)
      props1=props(22,:)';
      bar(thetgrid,props1)
      title('Figure 18.3: Posterior distribution when MLE=1')
      xlabel('Theta')
      ylabel('Proportion')
      