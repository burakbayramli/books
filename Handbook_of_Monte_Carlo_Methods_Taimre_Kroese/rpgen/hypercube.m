%hypercube.m
N = 200; % number of samples
n = 20; %dimension
x= zeros(N,n);
for t=1:N
    I = ceil(rand*n); %choose random position
    x(t+1,:) = x(t,:); %copy
    x(t+1,I) = ~x(t+1,I); %flip bit at position I
end
b = 0.5.^[1:n];
y = x*b';
hold on
plot(0:N,y,'.'), plot(0:N,y)
hold off
