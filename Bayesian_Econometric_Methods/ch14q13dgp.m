%For chapter 14, Question 13
%Simulate artificial panel dat6a set with intercept plus 2 explanatory variables
%Stochastic frontier model

n=100;
t=5;
tn=t*n;

theta = [1; .75; .25];
sigma=.2;
vz=2;
muz=-log(.85);
muterm=1/muz;

%stack data sets with orering such that all t observations on individual are together
%Generate artificial data on the explanatory variable
x=[ones(tn,1) rand(tn,1) rand(tn,1)];
y=zeros(n,1);
for i=1:n
%draw an inefficiency term
zdraw=gamm_rnd(1,1,.5*vz,.5*vz*muterm);
   y(1+t*(i-1):i*t,1)= x(1+t*(i-1):i*t,:)*theta+sigma*norm_rnd(eye(t))...
       -zdraw*ones(t,1);
end
data = [y x ];
save stoch_front.out data -ASCII;
