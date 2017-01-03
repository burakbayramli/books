%nm1p09: to plot the mulaw curve
clear, clf
x=[-1:.005:1];  
mu=[10 50 255];
for i=1:3
  [y,xmax]=mulaw(x,mu(i),1);
  plot(x,y,'b-', x,x0,'r-'), hold on 
  x0=mulaw_inv(y,mu(i),xmax); 
  discrepancy=norm(x-x0)
end
