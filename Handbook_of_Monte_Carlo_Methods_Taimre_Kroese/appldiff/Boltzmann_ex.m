function Boltzmann_ex
% Simple Boltzmann equation example
% Spatially homogeneous regime
N=10^3; d=3;% Number of particles; Dimension
T=10^0; % Final time
v=-log(rand(N,d));
rho=1;
w=rho.*ones(N,1)./N;
C=1; alpha=1; % Collision constants
pairs=nchoosek((1:1:N),2);
t=0;
hh=mean(sum(v.^3,2));
tt=t;
while t<=T
  qij=C.*((sum((v(pairs(:,1),:)-v(pairs(:,2),:)).^2,2)).^(0.5*alpha));
  sumqij=sum(qij);
  lambda=rho*sumqij/N;
  tau=-log(rand(1))/lambda;
  t=t+tau % Time of the new collision
  if t>T
    t=T; break;
  end
  p=[0;cumsum(qij./sumqij)];
  r=rand(1);    
  pidx=find(p>=r,1)-1; % Index of the colliding pair
  % Uniformly distributed on the Unit sphere in \R^3
  sigma=randn(1,3); sigma=sigma./sqrt(sum(sigma.^2)); 
  a=(v(pairs(pidx,1),:)+v(pairs(pidx,2),:))./2;
  b=(sum((v(pairs(pidx,1),:)-v(pairs(pidx,2),:)).^2)^(0.5))/2;
  vprime=a+sigma.*b;
  v1prime=a-sigma.*b;
  v(pairs(pidx,1),:)=vprime;
  v(pairs(pidx,2),:)=v1prime;
  tt=[tt,t];
  hh=[hh,mean(sum(v.^3,2))];
end
figure,plot(tt,hh,'k-')
