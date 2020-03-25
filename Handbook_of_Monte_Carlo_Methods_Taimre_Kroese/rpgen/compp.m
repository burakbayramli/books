%compp.m
T = 5; delta = 10^-6; epsilon = inf;
lambda = 4*(1/sqrt(delta) - 1/sqrt(epsilon));
X = []; tt  = []; t=0; x= 0;
while t < T
    a = -log(rand)/lambda;
    t = t + a;
    R = (rand < 0.5);
    U = rand;
    y = (2*R-1)*delta/(1-U + sqrt(delta/epsilon)*U)^2;
    x = x + y;
    X = [X,x];
    tt = [tt,t];
end
N = numel(tt);
hold on
for i=1:N-1
    line([tt(i),tt(i+1)],[X(i),X(i)],'Linewidth',1);
end
