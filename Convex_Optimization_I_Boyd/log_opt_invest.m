clear all

P = [3.5000    1.1100    1.1100    1.0400    1.0100;
     0.5000    0.9700    0.9800    1.0500    1.0100;
     0.5000    0.9900    0.9900    0.9900    1.0100;
     0.5000    1.0500    1.0600    0.9900    1.0100;
     0.5000    1.1600    0.9900    1.0700    1.0100;
     0.5000    0.9900    0.9900    1.0600    1.0100;
     0.5000    0.9200    1.0800    0.9900    1.0100;
     0.5000    1.1300    1.1000    0.9900    1.0100;
     0.5000    0.9300    0.9500    1.0400    1.0100;
     3.5000    0.9900    0.9700    0.9800    1.0100];

[m,n] = size(P);
x_unif = ones(n,1)/n; % uniform resource allocation

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSERT YOUR CODE HERE
% Solve the log-optimal investment problem,
% assuming all events are equiprobable.
% Store you result in the variable x_opt





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





% Generate random event sequences
rand('state',10);
N = 10;  % number of random trajectories 
T = 200; % time horizon
w_opt = []; w_unif = [];
for i = 1:N
    events = ceil(rand(1,T)*m);
    P_event = P(events,:);
    %w_opt = [w_opt [1; cumprod(P_event*x_opt)]];
    w_unif = [w_unif [1; cumprod(P_event*x_unif)]];
end

% Plot wealth versus time
figure
%semilogy(w_opt,'g')
%hold on
semilogy(w_unif,'r--')
grid
axis tight
xlabel('time')
ylabel('wealth')
