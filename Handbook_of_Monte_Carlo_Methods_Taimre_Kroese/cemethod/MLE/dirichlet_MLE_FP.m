function afp=dirichlet_MLE_FP(data,K)
%Compute Dirichlet MLE via a fixed-point technique
logpdata=mean(log(data),1);
afp=ones(1,K); afpold=-inf.*afp;
while sqrt(sum((afp-afpold).^2))>10^(-12)
   afpold=afp; s=sum(afpold);
   for k=1:K
     y=(psi(s)+logpdata(k));
     if y>=-2.22
         ak=exp(y)+0.5;
     else
         ak=-1/(y-psi(1));
     end
     akold=-inf;
     while abs(ak-akold)>10^(-12)
        akold=ak; ak=akold - ((psi(akold)-y)/psi(1,akold));
     end
     afp(k)=ak;
   end
end
