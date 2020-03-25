%SCMb.m
clear all
N=10^4; % Max sample size

g=@(Z) abs(exp(-Z.^2).*cos(3.*pi.*Z)).*...
    exp(-0.5.*((Z-1)./sqrt(2)).^2)./sqrt(2*pi*2);
h=@(Z,mu,sigma) (sigma>0)./(pi.*sigma.*(1+((Z-mu)./sigma).^2));
p=@(Z) 1./(pi.*(1+Z.^2)); % standard cauchy
f=@(x,Z) sum((g(Z)./p(Z)).*log(h(Z,x(1),x(2))));% St. Counterpart

Z=zeros(N,1); % Allocate Z
muhist=zeros(N,1); % Allocate space for estimated mus
sighist=zeros(N,1); % Allocate space for estimated sigmas
for i=1:N
    Z(i)=randn/randn; % Each Z_i is indep. with density p
    sol=fminsearch(@(x) -f(x,Z(1:i)),[1,2]); % Solve the SC
    muhist(i)=sol(1); sighist(i)=sol(2); % Update the histories
end
figure,plot((1:1:N),muhist,'k-','LineWidth',2)
figure,plot((1:1:N),sighist,'k-','LineWidth',2)



