%SA.m
% Initialization
n=10; % dimension of the problem
beta=0.99999; % Factor in geometric cooling
sigma=ones(1,n).*0.75; % Variances for the proposal
N=1; %Number of steps to perform of MH
maxits=10^6; % Number of iterations
xstar=10.*ones(1,n); eta=0.8; mu=0.1;
S=@(X) 1+sum(mu.*(X-xstar).^2+6.*(sin(2.*eta.*(X-xstar).^2)).^2+...
    8.*(sin(eta.*(X-xstar).^2)).^2);
T=10; % Initial temperature
a=-50;b=50; X=(b-a).*rand(1,n)+a; %Initialize X
Sx=S(X); % Score the initial sample

t=1; % Initialize iteration counter
while (t<=maxits)
    T=beta*T; % Select New Temperature    
    % Generate New State
    it=1;
    while (it<=N)
        Y=X+sigma.*randn(1,n);
        Sy=S(Y);
        alpha=min(exp(-(Sy-Sx)/T),1);
        if rand<=alpha
           X=Y; Sx=Sy;
        end
        it=it+1;
    end
    t=t+1; % Increment Iteration
end
[X,Sx,T] % Display final state, score, and temperature
