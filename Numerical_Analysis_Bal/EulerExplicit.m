function [t,X]=EulerExplicit(my_function,T,dT,X0);

%% Solve vector-valued ODEs by the Euler Explicit method

t=0:dT:T;         % grid
N=length(t)-1;    % number of intervals
sX0=length(X0);   % size of X0 
X=zeros(sX0,N+1); % initialization of solution vector
X(:,1)=X0;        % initial condition for X

%% Calculation of solution by Explicit Euler
for i=1:N        
    X(:,i+1)=X(:,i)+dT*feval(my_function,i*dT,X(:,i));
end;

%% Create column vectors for display and compatibility
%% with ode23 and ode45
t=t'; X=X';