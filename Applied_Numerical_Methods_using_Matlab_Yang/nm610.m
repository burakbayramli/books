%nm610: Euler method to solve a 1st-order differential equation
clear, clf
a=1; r=1;  y0=0; tf=2;
t=[0:0.01:tf];  yt=1-exp(-a*t);
plot(t,yt,'k'), hold on
klast=[8 4 2 3];   
h(1:3)=tf./klast(1:3); h(4)=2;   
y(1)=y0;
for itr=1:4
   for k=1:klast(itr)
      y(k+1)=(1-a*h(itr))*y(k) +h(itr)*r;
      plot([k-1 k]*h(itr),[y(k) y(k+1)],'b', k*h(itr),y(k+1),'ro')
      if k<4, pause; end
   end
end
