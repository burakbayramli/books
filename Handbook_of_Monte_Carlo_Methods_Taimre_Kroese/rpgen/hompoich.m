%hompoich.m
for i=1:6
   N = poissrnd(20);
   x = rand(N,2);
   k = convhull(x(:,1),x(:,2));
   [K,v] =  convhulln(x); %v is the area
   subplot(2,3,i);
   plot(x(k,1),x(k,2),'r-',x(:,1),x(:,2),'.')
end
