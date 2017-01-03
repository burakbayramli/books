function demoSampleHMM
%DEMOSAMPLEHMM demo of Gibbs sampling from a HMM versus exact result
H=2; V=2; T=10;
% make a HMM
lambda=1;
A=condp(rand(H,H).^lambda);
B=condp(rand(V,H));
a=condp(rand(H,1));

% draw some samples for v:
h(1)=randgen(a); v(1)=randgen(B(:,h(1)));
for t=2:T
    h(t)=randgen(A(:,h(t-1)));  v(t)=randgen(B(:,h(t)));
end
[logalpha,loglik]=HMMforward(v,A,a,B); logbeta=HMMbackward(v,A,B);
gamma=HMMsmooth(logalpha,logbeta,B,A,v) % exact marginal

% single site Gibbs updating
hsamp(:,1)=randgen(1:H,1,T);
hv=1:T; vv=T+1:2*T; % hidden and visible variable indices

num_samples=100;
for sample=2:num_samples
    h = hsamp(:,sample-1);
    emiss.table=B; emiss.variables=[vv(1) hv(1)];
    trantm.table=a; trantm.variables=hv(1);
    trant.table=A; trant.variables=[hv(2) hv(1)];
    h(1) = randgen(table(setpot(multpots([trantm trant emiss]),[vv(1) hv(2)],[v(1) h(2)])));
    
    for t=2:T-1
        trantm.table=A; trantm.variables=[hv(t) hv(t-1)];
        trant.table=A; trant.variables=[hv(t+1) hv(t)];
        emiss.table=B; emiss.variables=[vv(t) hv(t)];
        h(t) = randgen(table(setpot(multpots([trantm trant emiss]),[vv(t) hv(t-1) hv(t+1)],[v(t) h(t-1) h(t+1)])));
    end
    
    trantm.table=A; trantm.variables=[hv(T) hv(T-1)];
    emiss.table=B; emiss.variables=[vv(T) hv(T)];
    h(T) = randgen(table(setpot(multpots([trantm emiss]),[vv(T) hv(T-1)],[v(T) h(T-1)])));
    
    hsamp(:,sample)=h; % take the sample after a forward sweep through time
end
for t=1:T
    gamma_samp(:,t) = count(hsamp(t,:),H)/num_samples;
end
gamma_samp % sample marginal
fprintf('mean absolute error in the marginal estimate=%g\n',mean(abs(gamma(:)-gamma_samp(:))))