%regenmeth.m
clear all, p=0.25;q=1-p;
N=10000;
z = zeros(1,N);R = zeros(1,N);tau = zeros(1,N);
Rsum=0;regcount = 0;lastregtime = 1;
for i=2:N
    if rand<p
        z(i)=z(i-1)+1;
    elseif z(i-1) %if z is not zero
        z(i)=z(i-1)-1;
    end
     Rsum = Rsum + z(i); 
    if z(i)==0 %regeneration detected
        regcount = regcount + 1;
        R(regcount) = Rsum;
        tau(regcount) = i - lastregtime; 
        Rsum = 0;
        lastregtime = i;
    end
end
stairs(0:59,z(1:60)),hold on, plot(0:59,z(1:60),'.')
ell = mean(R)/mean(tau)
C = cov(R,tau);
s = sqrt(C(1,1) - 2*ell*C(1,2) + ell^2*C(2,2))
RE = s/mean(tau)/sqrt(N)
fprintf('ell %g ; 0.95 CI ( %g , %g ) \n', ...
             ell,ell*(1-1.96*RE),ell*(1+1.96*RE))
