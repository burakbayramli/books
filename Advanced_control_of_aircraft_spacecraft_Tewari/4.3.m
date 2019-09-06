global T;T=[0 1 2 3 4 5 6 7 10 15 20 25 30]';
global aT;aT=[-0.5 1 3 3.5 3.6 3.8 3.9 4 4.11 4.12 4.13 4.15 4.2
0.1 -1 -5 -5.5 -5.6 -5.8 -6.0 -5.9 -5.2 -4 -3.5 -3.6 -3.7
2.5 2 1 -1 -1.5 -2 -1.6 0.4 1.6 2.1 2.7 2.8 3]';
global kr;kr=.2;
global kv;kv=.7;
global M; M = [0; 0.2; 0.3; 0.4; 0.5; 0.6; 0.8; 0.9; 0.95; 1.05; 1.1; 1.2;
1.6; 2.0; 2.5; 3; 3.8; 5; 10; 99];
global CD;CD=[0.14264 0.14264 0.142728 0.145008 0.146688 0.152504 ...
0.169688
0.185448
0.20044
0.309536 0.30512 0.297168 ...
0.244784 0.207712 0.182912
0.163816
0.153904
0.1482 ...
0.144952 0.144952];
global S; S = 0.09; % reference base area, m2
global tb; tb=20; % burn-time (s)
global m0; m0=600; % initial mass (kg)
global mf; mf=80; % burn-out mass (kg)
[t,x]=ode45('missile_state',[0 20],[1000 2000 3000 100 -50 10 0 0 0 0 0 0]');
n=size(t,1);
v=[];d=[];f=[];miss=[];
for i=1:n
v(i,:)=norm(x(i,10:12));
end
[u,D,fT]=missile_state_u(t,x);
for i=1:n-1
d(i,:)=norm(D(i,:)); % drag
f(i,:)=norm(fT(:,i)); % thrust magnitude
miss(i,:)=norm(x(i,7:9)-x(i,1:3)); % miss distance
end
