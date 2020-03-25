%maze.m
n = 101
a = 0.5; b = 1/3;
P = [0, 1, 0, 0, 0, 0, 0, 0; a, 0, a, 0, 0, 0, 0, 0;
     0, b, 0, b, 0, 0, b, 0; 0, 0, a, 0, a, 0, 0, 0;
     0, 0, 0, b, 0, b, b, 0; 0, 0, 0, 0, 1, 0, 0, 0;
     0, 0, b, 0, b, 0, 0, b; 0, 0, 0, 0, 0, 0, 1, 0 ]
x = zeros(1,n);
x(1)= 3;
for t=1:n-1
    x(t+1) = min(find(cumsum(P(x(t),:))> rand));
end
hold on
plot(0:n-1,x,'.')
plot(0:n-1,x)
hold off

