%% clean up the workspace
clear all;
close all;
clc;

%% histogram
%{
In this example we draw a histogram of samples from the standard
normal distribution. The bars are normalized to correspond to an
approximate pdf. We color the bars blue if they correspond to
points that are at most one standard deviation from the mean, and
we color the bars red if they correspond to points more than one
standard deviation from the mean.
%}

% sample the normal distribution
N = 1e6;
x = randn(N,1);
bins = (-6.00):(0.10):(+6.00);
dx = bins(2) - bins(1);

% draw the blue bars
figure();
b = bar(bins , histc(x(abs(x) <= 1) , bins) / (N*dx) , 'histc');
set(b , 'FaceColor' , 'b');

% draw the red bars
hold on;
b = bar(bins , histc(x(abs(x) > 1) , bins) / (N*dx) , 'histc');
set(b , 'FaceColor' , 'r');

% draw the actual pdf
plot(bins , normpdf(bins) , 'g' , 'LineWidth' , 2);
hold off;

% miscellaneous formatting
xlim(6 * [-1 , +1]);
ylim([0 0.45]);
title('histogram')
xlabel('x');
ylabel('f(x)');

%% stairs
%{
A stairs plot provides a better representation of many discrete
signals compared to a standard plot. In this example, we plot a
sample sequence of server requests using the model from the server
sleep scheduling problem.
%}

% define the matrix P(i,j) = Prob(r(t+1) = 1|r(t-1) = i-1,r(t) = j-1)
P = [0.1 , 0.6 ; 0.3 , 0.9];

% define the matrix Q(i,j) giving the probability transition matrix
% on the Markov chain with state space {(0,0),(0,1),(1,0),(1,1)}
Q = [0.9 , 0.1 , 0.0 , 0.0 ; % state (0,0)
     0.0 , 0.0 , 0.4 , 0.6 ; % state (0,1)
     0.7 , 0.3 , 0.0 , 0.0 ; % state (1,0)
     0.0 , 0.0 , 0.1 , 0.9]; % state (1,1)

% compute the stationary distribution of the chain of server requests
pi_ss = [zeros(1,4) , 1] / [(eye(4)-Q) , ones(4,1)];

% sample the first two states from the stationary distribution
T = 100;
r = zeros(T+1,1);
z = find(rand() <= cumsum(pi_ss),1);
switch z
    case 1
        r([1 2]) = [0 0];
    case 2
        r([1 2]) = [0 1];
    case 3
        r([1 2]) = [1 0];
    case 4
        r([1 2]) = [1 1];
end

% finish generating the sample sequence
for t = 2:T
    r(t+1) = double(rand() < P(r(t-1)+1 , r(t)+1));
end

% plot the sample sequence
figure();
stairs(0:T , r , 'k');
ylim([0 1.25]);
title('stairs');
xlabel('t');
ylabel('r(t)');

%% subplot
%{
The subplot command is useful for generating multiple plots on a
single figure for the sake of comparison. In this example we plot the
state, feedback gain, input and disturbance for a sample trajectory
of a scalar system with an LQR controller.
%}

% generate a problem instance
T = 25;
a = randn(T,1);
b = randn(T,1);
W = abs(randn(T,1));
Q = 1 + abs(randn(T+1,1));
R = 1 + abs(randn(T,1));

% compute the LQR controller
P = [nan(T,1) ; Q(T+1)];
r = [nan(T,1) ; 0];
K = nan(T,1);
for t = T:(-1):1
    K(t) = -(R(t) + b(t)' * P(t+1) * b(t)) \ (b(t)' * P(t+1) * a(t));
    P(t) = Q(t) + K(t)' * R(t) * K(t) ...
         + (a(t) + b(t) * K(t))' * P(t+1) * (a(t) + b(t) * K(t));
    r(t) = r(t+1) + (1/2)*trace(P(t+1)*W(t));
end

% generate a sample trajectory
x = [randn() ; nan(T,1)];
u = nan(T,1);
w = nan(T,1);
for t = 1:T
    u(t) = K(t) * x(t);
    w(t) = sqrt(W(t)) * randn();
    x(t+1) = a(t)*x(t) + b(t)*u(t) + w(t);
end

% plot the state, feedback gain, input and disturbance power
m = 2; % number of rows of plots
n = 2; % number of columns of plots

% plot the state
figure();
subplot(m,n,1);
plot(0:T , x , 'k');
xlabel('t');
ylabel('x');
title('subplot');
xlim([0 T]);

% plot the feedback gain
subplot(m,n,2);
plot(0:(T-1) , K , 'k');
xlabel('t');
ylabel('K');
xlim([0 T]);

% plot the input
subplot(m,n,3);
plot(0:(T-1) , u , 'k');
xlabel('t');
ylabel('u');
xlim([0 T]);

% plot the disturbance power
subplot(m,n,4);
plot(0:(T-1) , W , 'k');
xlabel('t');
ylabel('w');
xlim([0 T]);

%% colormap
%{
Use a colormap to draw different trajectories of the gambler's ruin
problem using different colors. In this example, we use the hsv
colormap; for a full list of the available colormaps, type
doc colormap.
%}

% define the parameters of the gambler's ruin problem
p  = 0.5;  % probability of increasing wealth
X  = 100;  % wealth at which the gambler quits
x0 =  40;  % initial state

% simulate the gambler's ruin problem
N = 10;
x = cell(N,1);
for k = 1:N
    x{k} = x0;
    while true
        if rand() > p
            x{k} = [x{k} ; x{k}(end)-1];
        else
            x{k} = [x{k} ; x{k}(end)+1];
        end
        
        if x{k}(end) == 0 || x{k}(end) == X
            break;
        end
    end
end

% plot the trajectories
T = length(x{1})-1;
for k = 2:N
    T = max(T , length(x{k})-1);
end

figure();
color_map = hsv(N);
hold on;
    plot([0 T+1] , [0 0] , 'k');
    plot([0 T+1] , X * [1 1] , 'k');
    for k = 1:N
        plot(0:(length(x{k})-1) , x{k} , 'Color' , color_map(k,:));
        plot(length(x{k})-1 , x{k}(end) , 'Color' , 'k' , ...
             'Marker' , 'o' , 'MarkerFaceColor' , color_map(k,:));
    end
hold off;
box on;
xlim([0 T+1]);
ylim([-1 X+1]);
xlabel('t');
ylabel('x_t');
title('colormap');

%% stem plot
%{
A stem plot often provides a better representation of a sequence (such as
a probability mass function) than a line plot.
%}
n = 5;
p = rand(n,1);
p = p / sum(p);

figure();
stem(1:n , p , 'k');
xlim([0 n+1]);
ylim([0 1]);
xlabel('x');
ylabel('p(x)');
title('stem');

%% area plot
%{
An area plot can be used to show changes in the composition of a population
over time, such as the fraction of S/I/R individuals in an epidemic model.
%}
n = 100;
x = cumsum(abs(randn(n,1)));
y = cumsum(abs(randn(n,1)));
z = cumsum(abs(randn(n,1)));
s = x+y+z;

figure();
a = area(1:n , [x./s y./s z./s]);
color_map = hsv(length(a));
for k = 1:length(a)
    set(a(k) , 'FaceColor' , color_map(k,:));
end
ylim([0 1]);
xlabel('t');
ylabel('fraction of population');
title('area');

%% pcolor plot
%{
A pcolor (pseudocolor) plot is a rectangular array of cells with different
colors. This is useful for visualizing the state of a population in an
S/I/R model.
%}
n = 25;
w = rand(n,n);
X = 1 + (w > 1/3) + (w > 2/3);

figure();
pcolor(1:n , 1:n , X);
colormap(hsv(3));
axis off;
title('pcolor');