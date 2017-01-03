clear
t_0 = 0; t_f = 1; N = 320; M = 320;

[u,u_exact,xi,x,t] = nonl_explicit_upwind(t_0,t_f,M,N);

figure(1)
for j=0:32
  plot(x,u(:,1+j*10),'LineWidth',2)
  hold on
  plot(x,u_exact(:,1+j*10),':','LineWidth',2)
  axis([0,2*pi,0,1.4])
  title('dotted: exact, solid: approximiate','FontSize',16);
  figure(1)
  hold off
  pause(1)
end

pause

figure(2)
plot(xi,t,'LineWidth',2)
hold on
plot(pi/2+.8*t,t,':','LineWidth',2)
hold off
xlabel('location of discontinuity','FontSize',16);
ylabel('time','FontSize',16);
title('solid: approximate, dotted: exact','FontSize',16)
figure(2)






    


