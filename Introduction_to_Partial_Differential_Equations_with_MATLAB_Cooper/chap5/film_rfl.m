x = 0:.1 :10;

for j = 1:40
    t= (j-1)*.25;
    u = bigg(x+t) + bigg(t-x);
    plot(x,u)
    axis([0 10 -1.5 1.5])

    Reflect(j) = getframe;
end

movie(Reflect)

     



