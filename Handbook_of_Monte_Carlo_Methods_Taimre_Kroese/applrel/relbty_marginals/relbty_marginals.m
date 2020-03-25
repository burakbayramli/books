clear all,clc
global  GRAPH
[E,p,paths,cuts]=data(2); % load Graph object 
GRAPH.E=E; GRAPH.cuts=cuts; GRAPH.paths=paths; 

p=p*0+(1-0.1^3);
GRAPH.sig=-1./norminv(1-p'); % Gaussian latent variables 
rho=0.1; 
Gamma=1; m=length(p); [c_pilot,gam,X]=adam(10^4,Gamma,rho); 

I=conditional_pdfs(X,1);
w=mean(I); w(w==0)=0.1;
sig=GRAPH.sig(1);
M=10^6;  ell=zeros(M,1);
for k=1:M
   x=conditional_rnd(w,sig,1); 
   if S(x)>1
    ell(k)=W(x,w,sig,1);
   end
end
l=mean(ell)
std(ell)/mean(ell)/sqrt(M)
