%zip.m
n=100; p=.3; lambda=2;
% generate ZIP random variables
data=poissrnd(lambda,n,1).*(rand(n,1)<p); 
% now try to recover the ZIP parameters from the data
P=rand; % starting guess for p
lam=gamrnd(1,1); % starting guess for lambda
r=(rand(n,1)<P); % starting guess for  r
Sum_data=sum(data);
gibbs_sample=zeros(10^5,2);
% apply the Gibbs sampler
for k=1:10^5
   Sum_r=sum(r);
   lam=gamrnd(1+Sum_data,1/(1+Sum_r)); 
   P=betarnd(1+Sum_r,n+1-Sum_r);
   prob=exp(-lam)*P./(exp(-lam)*P+(1-P)*(data==0));
   r=(rand(n,1)<prob);
   gibbs_sample(k,:)=[P,lam];   
end
% 95% probability interval for lambda
prctile(gibbs_sample(:,2),[2.5,97.5]) 
% 95% probability interval for p
prctile(gibbs_sample(:,1),[2.5,97.5]) 
