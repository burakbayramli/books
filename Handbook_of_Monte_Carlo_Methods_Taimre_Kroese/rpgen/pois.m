%pois.m
T = 50;
t = 0; n = 0;
tt = [t];
while t < T
    t = t - log(rand);
    if (rand < sin(t)^2)
        tt = [tt,t];
        n = n+1;
    end
end
nn = 0:n;
for i =1:n
    line([tt(i),tt(i+1)],[nn(i),nn(i)],'Linewidth',2);
end
