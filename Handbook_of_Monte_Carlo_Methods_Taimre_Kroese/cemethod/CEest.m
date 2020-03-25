%CEest.m
f='minbeta'; % performance function name
gam=0.999; % Desired level parameter
n=5;  % dimension of problem
N=10^5; % sample size
rho=0.01;
N1=10^6; % Final estimate sample size
N_el=round(N*rho);  % elite sample size
u=0.5.*ones(1,n); % Nominal reference parameter in Beta(1,u/(1-u)) 
v=u; gt=-inf; % Initialize v and gamma
maxits=10^5; % Fail-safe stopping after maxits exceeded
it=0; tic
while (gt<gam)&(it<maxits)
  it=it+1;
  % Generate and score X's
  X=rand(N,n).^(1./repmat(v./(1-v),N,1)); % Beta(1,v/(1-v)) 
  S=feval(f,X); [U,I]=sort(S);  % Score & Sort
  % Update Gamma_t
  gt=U(N-N_el+1);
  if gt>gam, gt=gam; N_el=N-find(U>=gam,1)+1; end
  Y=X(I(N-N_el+1:N),:);   
  % Calculate likelihood ratios and update v
  W=prod(repmat((u./(1-u))./(v./(1-v)),N_el,1).*...
      Y.^(repmat((u./(1-u))-(v./(1-v)),N_el,1)),2);
  v=sum(repmat(W,1,n).*Y)./sum(repmat(W,1,n));
  [gt,v] % Display gamma and v
end
% Final estimation step
X1=rand(N1,n).^(1./repmat(v./(1-v),N1,1));
S1=feval(f,X1);
W1=prod(repmat((u./(1-u))./(v./(1-v)),N1,1).*...
    X1.^(repmat((u./(1-u))-(v./(1-v)),N1,1)),2);
H1=(S1>=gam);
ell=mean(W1.*H1);
re=sqrt((mean((W1.*H1).^2)/(ell^2))-1)/sqrt(N1);
% Display final results
time=toc; disp(time), disp(v), disp(ell), disp(re)
ell_true=(1-gam)^n;disp(ell_true) % Display true quantity