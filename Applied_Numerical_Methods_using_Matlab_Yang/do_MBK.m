%do_MBK
clear, clf
t0=0; tf=10; x0=[0 0];
[t1,x]=ode_Ham('f_MBK',[t0 tf],x0);
dt=t1(2)-t1(1);
for n=1:length(t1)
  u(n)= udu_MBK(t1(n));
end   
figure(1), clf
animation=1;
if animation
  figure(2), clf
  draw_MBK(5,1,x(1,2),u(1))
  axis([-2 2 -1 14]), axis('equal')
  pause
  for n=1:length(t1)
    clf, draw_MBK(5,1,x(n,2),u(n),'b')
    axis([-2 2 -1 14]), axis('equal')
    pause(dt)
    figure(1)
    plot(t1(n),u(n),'r.', t1(n),x(n,2),'b.')
    axis([0 tf -0.2 1.2]), hold on
    figure(2)
  end  
  draw_MBK(5,1,x(n,2),u(n))
  axis([-2 2 -1 14]), axis('equal')
end
figure(1)
plot(t1,u,'r:', t1,x(n,2),'b:')
