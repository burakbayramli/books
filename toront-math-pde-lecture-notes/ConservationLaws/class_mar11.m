
% Do some runs with Burger's Equation
t_0 = 0; t_f = 4; N = 200; M = 300;
[u1,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
[u2,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
[u3,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);

for j=1:M+1
    plot(x,u1(:,j))
    hold on
    plot(x,u2(:,j),'r')
    plot(x,u3(:,j),'g')
    figure(1)
    hold off
end
    

figure(1)
clf
plot(x,u1(:,1),'LineWidth',2);
hold on
plot(x,u1(:,M+1),'LineWidth',2);
plot(x,u2(:,M+1),'--','LineWidth',2);
plot(x,u3(:,M+1),'-.','LineWidth',2);
hold off
xlabel('x','FontSize',16);
ylabel('u','FontSize',16);
axis([-pi,pi,-.1,1.3])
% print -dps burgers_shock.ps
figure(1)

figure(2)
clf
hold on
plot(x,u1(:,M+1),'LineWidth',2);
plot(x,u2(:,M+1),'--','LineWidth',2);
plot(x,u3(:,M+1),'-.','LineWidth',2);
axis([1.5,2.5,-.1,1.3])
xlabel('x','FontSize',16);
ylabel('u','FontSize',16);
hold off
% print -dps burgers_shock_closeup.ps
figure(2)

for j=1:M+1
  [cross11(j),cross12(j),min11(j),min12(j)] = extract(x,u1(:,j));
  [cross21(j),cross22(j),min21(j),min22(j)] = extract(x,u2(:,j));
  [cross31(j),cross32(j),min31(j),min32(j)] = extract(x,u3(:,j));
end

figure(3)
clf
plot(cross11,t,'LineWidth',2);
hold on
plot(cross21,t,'--','LineWidth',2);
plot(cross31,t,'-.','LineWidth',2);
xlabel('x','FontSize',16);
ylabel('t','FontSize',16);
axis([0,2,0,4])
% print -dps shock_location.ps
hold off


figure(4)
clf
plot(cross11,t,'LineWidth',2);
hold on
plot(cross21,t,'--','LineWidth',2);
plot(cross31,t,'-.','LineWidth',2);
axis([1.28,1.38,2*1.28,2*1.38])
xlabel('x','FontSize',16);
ylabel('t','FontSize',16);
% print -dps shock_location_closeup.ps
hold off

slope1 = diff(cross11)./diff(t);
slope2 = diff(cross21)./diff(t);
slope3 = diff(cross31)./diff(t);
% figure(4)
% clf
% plot(slope1)
% hold on
% plot(slope2,'r')
% plot(slope3,'c')
% hold off
mean(slope1(M/2:M))
% ans =
%    4.9979e-01

mean(slope2(M/2:M))
% ans =
%    5.0022e-01

mean(slope3(M/2:M))
% ans =
%    4.9995e-01

% Do some runs with Burger's Equation, looking for rarefaction wave
t_0 = 0; t_f = 4; N = 200; M = 400;
% this has the initial data which is stationary
[v,x,t] = conservative_scheme(@shock1,@F_eu,t_0,t_f,M,N,2);
% this has the initial data which leads to rarefaction wave
[u,x,t] = conservative_scheme(@shock2,@F_eu,t_0,t_f,M,N,2);

for j=1:M+1
    plot(x,v(:,j))
    figure(1)
end

figure(1)
% plot solutions at times k/4 for k=0..8
plot(x,u(:,1))
hold on
plot(x,u(:,25+1))
plot(x,u(:,2*25+1))
plot(x,u(:,3*25+1))
plot(x,u(:,4*25+1))
plot(x,u(:,5*25+1))
plot(x,u(:,6*25+1))
plot(x,u(:,7*25+1))
plot(x,u(:,8*25+1))
axis([-pi,pi,-1.1,1.1])
xlabel('x','FontSize',16);
ylabel('u','FontSize',16);
% print -dps rarefaction_fan.ps
figure(1)

figure(2)
clf
% plot solutions at time 1.
plot(x,u(:,101))
hold on
plot(x,v(:,101),'--')
axis([-pi,pi,-1.1,1.1])
xlabel('x','FontSize',16);
ylabel('u,v','FontSize',16);
% print -dps alternate_initial_data.ps
figure(2)
