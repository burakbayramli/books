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

m,n = size(P);
x_unif = ones(n)/n; # uniform resource allocation

using Convex, SCS

#################################################################
# INSERT YOUR CODE HERE
# Solve the log-optimal investment problem
# assuming all events are equiprobable.
# Store your result in the variable x_opt

# Find log-optimal investment policy

#################################################################

using PyPlot, Distributions

srand(10);
N = 10; # number of random trajectories
t = 200; # time horizon
w_opt = zeros(N,t+1);
w_unif = zeros(N,t+1);
for i = 1:N
     events = vec(ceil(rand(1,t)*m))
     events = round(Int64, events)
     P_event = P[events,:];
     w_opt[i, :] = [1.0 ; cumprod(P_event*x_opt)];
     w_unif[i, :] = [1.0 ; cumprod(P_event*x_unif)];
end

# Plot wealth versus time
figure();
hold("true");
for i = 1:N
     semilogy(1:t+1, vec(w_opt[i,:]), "g", linestyle="--");
     semilogy(1:t+1, vec(w_unif[i,:]), "r");
end
axis([0, t, 0, 1000])
xlabel("time");
ylabel("wealth");
savefig("log_opt_invest.eps");
