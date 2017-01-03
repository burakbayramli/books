%Stochastic frontier model for panel data
%program which does exercise 13 of Chapter 14
%to create artificial data run ch14q13dgp.m
%Gibbs sampling for independent Normal-Gamma prior + hierarchical prior for ineff's
%The program ch14q6dgp.m artificially generates data used here
%Load data into y and x (all stacked by individual)

load stoch_front.out;
tn=size(stoch_front,1);
t=5;
n=100;
y=stoch_front(:,1);
x=stoch_front(:,2:4);
k=size(x,2);


%--------
%Define the prior hyperparameters
%--------
%for constant coefficients, Normal with mean mu_gamma and var V_gamma
mu_beta = zeros(k,1);
mu_beta(2,1)=.5;
mu_beta(3,1)=.5;
V_beta = (.25^2)*eye(k);
V_beta(1,1)=100;
V_binv=inv(V_beta);

%for error precision use Gamma prior with mean h02 and v0=d.o.f. 
v0 = 0;
h02=25;
s02 = 1/h02;

%For mu(z)-inverse use Gamma prior with mean mu_z-inverse and dof-v_z
v_z=2;
mu_z=-log(.5);

%Do OLS and related results (assuming no heterogeneity) to get starting values
bols = inv(x'*x)*x'*y;
s2 = (y-x*bols)'*(y-x*bols)/(tn-k);
%choose a starting value for h
hdraw=1/s2;
%choose a starting value for beta
bdraw=bols;
%choose a starting value for mu(z)
muzdraw=-log(.5);
zdraw=muzdraw*ones(n,1);
%Calculate a few quantities outside the loop for later use
xsquare=x'*x;
v1=v0+tn;
vz=2*n + v_z;
v0s02=v0*s02;
xtxsum=zeros(k,k);
xtysum=zeros(k,1);
for i=1:n
    xtxsum=xtxsum + x((i-1)*t+1:i*t,:)'*x((i-1)*t+1:i*t,:); 
    xtysum=xtysum + x((i-1)*t+1:i*t,:)'*y((i-1)*t+1:i*t,:); 
end

%store all draws in the following matrices
%initialize them here
b_=[];
h_=[];
muz_=[];
effmean=zeros(n,1);
effsd=zeros(n,1);
%Specify the number of replications
%number of burnin replications
s0=25;
%number of retained replications
s1=250;
s=s0+s1;

%Now start Gibbs loop

for irep = 1:s
    irep
    %Draw from z-i s conditional on other parameters
    
    zterm = 1/(t*hdraw*muzdraw);
    zvar=1/(t*hdraw);
    for i = 1:n
        xmean=mean(x((i-1)*t+1:i*t,:));
        ymean=mean(y((i-1)*t+1:i*t,:));
        zmean = xmean*bdraw - ymean -zterm;
     zdraw(i,1)=truncnorm_rnd(zmean,zvar,0);
    end   
 
    %Draw from Beta conditional on other parameters
    xtzsum=zeros(k,1);
    for i=1:n
       xtzsum=xtzsum + x((i-1)*t+1:i*t,:)'*(zdraw(i,1)*ones(t,1)); 
    end
    bvarinv = V_binv + hdraw*xtxsum;
    bvar=inv(bvarinv);
    bmean = bvar*(V_binv*mu_beta + hdraw*(xtysum +xtzsum));
    bdraw=bmean+norm_rnd(bvar);
    
    
    %draw from h conditional on other parameters
    s12=0; 
        for i = 1:n
        xuse=x((i-1)*t+1:i*t,:);
        yuse=y((i-1)*t+1:i*t,1);
           s12 = s12+ (yuse-xuse*bdraw + zdraw(i,1)*ones(t,1))'*...
           (yuse-xuse*bdraw + zdraw(i,1)*ones(t,1));
    end 
    s12= (s12 + v0s02)/v1;
    hdraw=gamm_rnd(1,1,.5*v1,.5*v1*s12);
    
  %draw from mu(z)-inverse conditional on other parameters
  meanmuz=(sum(zdraw) + mu_z)/(n+.5*v_z);
    muzinvdraw=gamm_rnd(1,1,.5*vz,.5*vz*meanmuz);
    muzdraw=1/muzinvdraw;
    
    
    if irep>s0
        %after discarding burnin, store all draws
        b_ = [b_ bdraw];
        h_ = [h_ hdraw];
        muz_=[muz_ muzdraw];
        effmean=effmean + exp(-zdraw);
        effsd=effsd + (exp(-zdraw)).^2;
        
    end
end
effmean=effmean./s1;
effsd=effsd./s1;
effsd=sqrt(effsd - effmean.^2);

alldraws = [b_' h_' muz_'];
%The function momentg is taken from LeSage's toolbox
%it inputs all Gibbs draws and produces posterior
%mean, standard deviation, nse and rne
%it calculates what Geweke calls S(0) in various ways
%see momentg.m for more details
result = momentg(alldraws);
means=[result.pmean]';
stdevs=[result.pstd]';
nse=[result.nse]';
nse1=[result.nse1]';
nse2=[result.nse2]';
nse3=[result.nse3]';


'Posterior results'
'number of burnin replications'
s0
'number of included replications'
s1

'Posterior means, std. devs and nse for parameters'
'Parameters ordered as beta, error precision, mu(z)'
[means stdevs nse]

'Posterior mean and standard deviation of efficiencies'
[effmean effsd]



figure(1)
hist(effmean,25)
title('Figure 7.5: Histogram of Posterior Means of Efficiencies')
xlabel('Efficiency')

end