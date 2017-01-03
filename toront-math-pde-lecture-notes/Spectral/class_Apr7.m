N = 128;
dx = 2*pi/N;
x = 0:dx:2*pi-dx;
u0 = cos(x);
 
t_0 = 0;
t_f = 1.5;
M = 5000;
 
D = 1/16;
[u6,x,t,kk,amp6] = burgers_ie(u0,D,t_0,t_f,M,N);
D = 1/32;
[u7,x,t,kk,amp7] = burgers_ie(u0,D,t_0,t_f,M,N);
D = 1/64;
[u8,x,t,kk,amp8] = burgers_ie(u0,D,t_0,t_f,M,N);
 
N = 2*N;
dx = 2*pi/N;
x = 0:dx:2*pi-dx;
u0 = cos(x);
D = 1/128;
[u9,x1,t,kk1,amp9] = burgers_ie(u0,D,t_0,t_f,M,N);
D = 0;
[u,x1,t,kk1,amp] = burgers_ie(u0,D,t_0,t_f,M,N);


% this is the D=0 solution
figure(1)
for j=0:40, 
    jj = j*125+1;
    t(jj)
    subplot(2,1,1)
    plot(x1,u(:,jj)), axis([0,2*pi,-1.1,1.1]), 
    title('D = 0');
    subplot(2,1,2)
    plot(kk1,amp(:,jj)), axis([0,N/2,-18,2])
    figure(1), pause(1) 
end

% this is the D=1/16 solution
figure(1)
for j=0:40, 
    jj = j*125+1;
    subplot(2,1,1)
    plot(x,u1(:,jj)), axis([0,2*pi,-1.1,1.1]), 
    title('D = 1/16');
    subplot(2,1,2)
    plot(kk,amp1(:,jj)), axis([0,max(kk),-18,2])
    figure(1), pause(1) 
end


% this is the D=1/16 solution
figure(1)
for j=0:40, 
    jj = j*125+1;
    subplot(2,1,1)
    plot(x,u6(:,jj)), axis([0,2*pi,-1.1,1.1]), 
    title('D = 1/16');
    subplot(2,1,2)
    plot(kk,amp6(:,jj)), axis([0,max(kk),-18,2])
    figure(1), pause(1) 
end


% this is the D=1/32 solution
figure(1)
for j=0:40, 
    jj = j*125+1;
    subplot(2,1,1)
    plot(x,u7(:,jj)), axis([0,2*pi,-1.1,1.1]), 
    title('D = 1/32');
    subplot(2,1,2)
    plot(kk,amp7(:,jj)), axis([0,max(kk),-18,2])
    figure(1), pause(1) 
end



% this is the D=1/64 solution
figure(1)
for j=0:40, 
    jj = j*125+1;
    subplot(2,1,1)
    plot(x,u8(:,jj)), axis([0,2*pi,-1.1,1.1]), 
    title('D = 1/64');
    subplot(2,1,2)
    plot(kk,amp8(:,jj)), axis([0,max(kk),-18,2])
    figure(1), pause(1) 
end


figure(1)
% this is the D=1/128 solution
for j=0:40, 
    jj = j*125+1;
    subplot(2,1,1)
    plot(x1,u9(:,jj)), axis([0,2*pi,-1.1,1.1]), 
    title('D = 1/128');
    subplot(2,1,2)
    plot(kk1,amp9(:,jj)), axis([0,max(kk1),-18,2])
    figure(1), pause(1) 
end

t(3334)
figure(2)
clf
% D=0
plot(x1,u(:,3334),'w','LineWidth',2);
hold on
% plot(x1,u9(:,3334),'y','LineWidth',2);
% D=1/64
plot(x,u8(:,3334),'c','LineWidth',2);
% D=1/32
plot(x,u7(:,3334),'r','LineWidth',2);
% D=1/16
plot(x,u6(:,3334),'y','LineWidth',2);
title('D=0,1/64,1/32,1/16 solutions at t=.9999','FontSize',16)
axis([0,3,-1.1,1.1])

