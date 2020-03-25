%CESAT.m
%Assumes *sparse* A is loaded in the workspace
[m,n]=size(A); % Dimensions of the problem
maxits=10^3; epsilon=1e-3;
N=10^4; rho=0.1; Ne=ceil(rho*N);
alpha=0.5; % Smoothing parameter
p = 0.5*ones(1,n); it=0; best=-inf; xbest=[];
Smaxhist=zeros(1,maxits); % Allocate history memory
Sminhist=zeros(1,maxits);
while (max(min(p,1-p)) > epsilon) && (it < maxits) && (best<m)
  it = it + 1;
  x = double((rand(N,n) < repmat(p,N,1)));
  SX = sC(A,x);
  [sortSX,iX] = sortrows([x SX],n+1);
  indices=iX(N- Ne + 1:N); 
  if sortSX(N,n+1)>best
	best=sortSX(N,n+1);	xbest=sortSX(N,1:n);
  end
  Smaxhist(it)=sortSX(N,n+1);Sminhist(it)=sortSX(N-Ne+1,n+1);
  p=alpha.*mean(sortSX(indices,1:n))+(1-alpha).*p;  
  disp([it,sortSX(N,n+1),sortSX(N-Ne+1,n+1),p])
end
disp([best xbest])
figure,plot((1:1:it),Smaxhist,'r-',(1:1:it),Sminhist,'k-')