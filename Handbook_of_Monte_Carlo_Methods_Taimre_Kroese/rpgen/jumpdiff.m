%jumpdiff.m
T = 1; nsteps = 10^3; h = T/nsteps;
lambda = 7; a = 0; b = 1; %Jump Parameters
gamma = 5;  sigma = 2; %BM Parameters
%Generate BM increments
dX = gamma*h + sigma*sqrt(h)*randn(nsteps,1);
dX(1) = 0;
%Generate Jump Process Part
N = poissrnd(lambda);
jumpidx = zeros(N,1); jumpsize = zeros(N,1);
for i = 1:N
    jumpidx(i) = ceil(rand*T*nsteps);
    jumpsize(i) = a + b*randn;
    dX(jumpidx(i)) = dX(jumpidx(i)) + jumpsize(i);
end
t = h:h:T;
X = cumsum(dX);
if N==0
    plot(t,X,'k-')
else
    jidx=sort(jumpidx);
    plot(t(1:jidx(1)-1),X(1:jidx(1)-1),'k-'),hold on
    for k=2:N
	plot(t(jidx(k-1):jidx(k)-1),X(jidx(k-1):jidx(k)-1),'k-')
    end
    plot(t(jidx(N):nsteps),X(jidx(N):nsteps),'k-'),hold off
end

