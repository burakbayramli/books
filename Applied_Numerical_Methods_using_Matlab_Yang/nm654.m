%nm654.m
% to solve a stiff differential eq. called Van der Pol equation
clear, clf
global mu
mu=25, t0=0; tf=100; tspan=[t0 tf]; x0=[2 0];
[tH1,xH1]=ode_Ham('df_van',tspan,x0,8700);
subplot(321), plot(tH1,xH1)
tic,[tH2,xH2]=ode_Ham('df_van',tspan,x0,9000); time_Ham=toc %5500
subplot(322), plot(tH2,xH2)
tic,[t45,x450]=ode45('df_van',tspan,x0); time_o45=toc
tic,[t23,x230]=ode23('df_van',tspan,x0); time_o23=toc
subplot(323), plot(t45,x450)
mu=200, tf=400; tspan=[t0 tf];
tic,[t45,x45]=ode45('df_van',tspan,x0); time(1)=toc;
tic,[t23,x23]=ode23('df_van',tspan,x0); time(2)=toc;
subplot(324), plot(t45,x45, t23,x23), hold on
tic,[t15s,x15s]=ode15s('df_van',tspan,x0); time(3)=toc;
tic,[t23s,x23s]=ode23s('df_van',tspan,x0); time(4)=toc;
tic,[t23t,x23t]=ode23t('df_van',tspan,x0); time(5)=toc;
tic,[t23tb,x23tb]=ode23tb('df_van',tspan,x0); time(6)=toc;
disp('   ode45     ode23      ode15s    ode23s    ode23t    ode23tb')
time
plot(t15s,x15s), pause, axis([t0 tf -3 2])
hold on, pause
plot(t23s,x23s), pause
plot(t23t,x23t), pause
plot(t23tb,x23tb), pause
%plot(t15s,x15s, t23s,x23s, t23t,x23t, t23tb,x23tb) 
subplot(325), plot(x450(:,1),x450(:,2))
subplot(326), plot(x45(:,1),x45(:,2),'b', x45(:,1),x45(:,2),'r')

