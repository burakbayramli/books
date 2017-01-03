function demoSumprodGaussCanonLDS
%DEMOSUMPRODGAUSSCANONLDS Sum-Product algorithm test on a LDS
figure
H=2; % dimension of each latent variable
V=3; % dimension of observations
hvars{1}=1; hvars{2}=2; hvars{3}=3;
A=0.99*orth(randn(H,H)); % stationary transition matrix
tmp=1*rand(H); SigmaH=tmp*tmp'; 
invSigmaH=inv(SigmaH);
priormean=randn(H,1);
meanH=zeros(H,1); meanV=zeros(V,1); 
% canonical form of the transition distribution:
trantable(1:H,1:H)=invSigmaH;
trantable(1:H,H+1:2*H)=-invSigmaH*A;
trantable(H+1:2*H,1:H)=-A'*invSigmaH;
trantable(H+1:2*H,H+1:2*H)=A'*invSigmaH*A;

v=randn(V,V); % observations
B=randn(V,H); tmp=randn(V,V); SigmaV=tmp*tmp'; invSigmaV=inv(SigmaV);

% Form the canonical potentials on the hidden variables alone, given the observations
poth(1).variables=hvars{1};
poth(1).table.invcovariance=invSigmaH;
poth(1).table.invmean=invSigmaH*priormean;
poth(1).table.logprefactor=0;
poth(1).table.type='GaussianCanonical';
poth(1).table.dim=H;

for t=2:3
    poth(t).variables=[hvars{t} hvars{t-1}];
    poth(t).table.invcovariance=trantable;
    poth(t).table.invmean=zeros(2*H,1);
    poth(t).table.logprefactor=0;
    poth(t).table.type='GaussianCanonical';
    poth(t).table.dim=H;
end

for t=1:3 % the visible variables contribute a canonical potential on each hidden variable:
    potv(t).variables=hvars{t};
    potv(t).table.invcovariance=B'*invSigmaV*B;
    potv(t).table.invmean=B'*invSigmaV*v(:,t);
    potv(t).table.logprefactor=0;
    potv(t).table.type='GaussianCanonical';
    potv(t).table.dim=H;
end
AFG = FactorGraph([poth potv]); drawFG(AFG);
[marg mess]=sumprodFG([poth potv],AFG,[]);
[f,F,g,G,Gp]=LDSsmooth(v,A,B,SigmaH,SigmaV,SigmaH,priormean,meanH,meanV);

fprintf(1,'LDS smoothed inference.\nSumproduct algorithm checked against the RTS method:\n')
for i=1:3
    fprintf(1,'\nvariable %d:\n',i);
    disp(['mean (factor graph method): ',num2str(marg(i).table.mean')])
    disp(['mean (raw integral method): ',num2str(g{t}')])
    disp(['covariance(factor graph method): ',num2str(marg(i).table.covariance(:)')])
    disp(['covariance(raw integral method): ',num2str(G{t}(:)')])
end