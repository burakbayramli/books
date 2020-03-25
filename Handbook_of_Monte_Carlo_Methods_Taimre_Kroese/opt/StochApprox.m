% StochApprox.m
maxits=10^4; % number of iterations
n=10^2; % dimension
N=10^1; % number of trials
mu=(n:-1:1); rmu=repmat(mu,N,1); % problem data
L=zeros(N,n); R=L; % allocate space for the central diff. estimator

c=1; % constant for the step size
delta = 1; % constant for the FD sequence 
betat=@(t) c./t; % step size functions
deltat=@(t) delta/t.^(1/6); % difference interval function

xrm=10.*n.*ones(1,n); % initial Robbins-Monro iterate
xkw=10.*n.*ones(1,n); % initial Kiefer-Wolfowitz iterate
xkwCRV=10.*n.*ones(1,n); % inital Kiefer-Wolfowitz iterate w. CRV

% allocate space for the convergence history of each iterate
rmhist=zeros(1,maxits);
kwhist=zeros(1,maxits);
kwCRVhist=zeros(1,maxits);
% compute initial distance to optimal solution
rmhist(1)=sqrt(sum((xrm-mu).^2));
kwhist(1)=sqrt(sum((xkw-mu).^2));
kwCRVhist(1)=sqrt(sum((xkwCRV-mu).^2));

t=1; % iteration Counter
while (t<maxits)
    % RM gradient est.
    xi=rmu+randn(N,n);
    grm=mean(2.*(repmat(xrm,N,1)-xi),1); % unbiased est.
    % KW gradient est.
    xiL=rmu+randn(N,n);
    xiR=rmu+randn(N,n);
    xkwN=repmat(xkw,N,1);
    e1=zeros(1,n);e1(1)=deltat(t)/2;
    ekN=repmat(e1,N,1);
    for k=1:n
        L(:,k)=sum((xiL-(xkwN+ekN)).^2,2);
        R(:,k)=sum((xiR-(xkwN-ekN)).^2,2);
        ekN=circshift(ekN,[0 1]);
    end
    gkw=mean((L-R)./deltat(t),1);
    % KW gradient est. with CRV
    xiL=rmu+randn(N,n);
    xiR=xiL; % practical CRV
    xkwCRVN=repmat(xkwCRV,N,1);
    for k=1:n
        L(:,k)=sum((xiL-(xkwCRVN+ekN)).^2,2);
        R(:,k)=sum((xiR-(xkwCRVN-ekN)).^2,2);
        ekN=circshift(ekN,[0 1]);
    end
    gkwCRV=mean((L-R)./deltat(t),1);
    % Update Iterates
    xrm=xrm-betat(t).*grm;
    xkw=xkw-betat(t).*gkw;
    xkwCRV=xkwCRV-betat(t).*gkwCRV;
    % increase iteration counter and record new distance to optimum
    t=t+1;
    rmhist(t)=sqrt(sum((xrm-mu).^2));
    kwhist(t)=sqrt(sum((xkw-mu).^2));
    kwCRVhist(t)=sqrt(sum((xkwCRV-mu).^2));
end
% plot the results
tt=(1:1:(maxits)); 
figure,semilogy(tt,rmhist,'k-',tt,kwhist,'b-',tt,kwCRVhist,'r-',...
    'Linewidth',1.5)
