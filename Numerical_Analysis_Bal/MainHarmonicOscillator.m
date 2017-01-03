T=20;      %% Final time
dT=0.1;    %% Time step
X0=[0 1]'; %% Initial Condition

%%% Matlab preprogrammed ODE solvers
[t,y]=ode23(@HarmonicOscillator,[0 T],X0);
%[s,z]=ode45(@HarmonicOscillator,[0 T],X0);

%%% Explicit Euler Scheme 
[s,z]=EulerExplicit(@HarmonicOscillator,T,dT,X0);

%%% Vector of plotted times
tplot=1:1:length(t); splot=1:1:length(s);
%Npts=100;
%tplot=length(t)-Npts:1:length(t)';
%splot=length(s)-Npts:1:length(s)';

%%% Plot of first variable as a function of time
figure(1);
plot(t(tplot),y(tplot,1),'b');
hold on;
plot(s(splot),z(splot,1),'r');
hold off;

%%% Plot in phase space (y_1(t),y_2(t)).
figure(2);
plot(y(:,1),y(:,2),'b');
hold on;
plot(z(:,1),z(:,2),'r');
hold off;