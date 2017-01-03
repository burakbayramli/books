%nm7p02 to minimize an objective ftn f(x) by the Newton method  
f='f7p02'; g='g7p02';
l=[-4 -4]; u=[4 4];
x1=l(1):.25:u(1); x2=l(2):.25:u(2); [X1,X2]=meshgrid(x1,x2); 
for m=1:length(x1)
   for n=1:length(x2), F(n,m)=feval(f,[x1(m) x2(n)]); end
end
figure(1), clf, mesh(X1,X2,F)
figure(2), clf, 
contour(x1,x2,F,[-125 -100 -75 -50 -40 -30 -25 -20 0 50])
... ... ... ... 
