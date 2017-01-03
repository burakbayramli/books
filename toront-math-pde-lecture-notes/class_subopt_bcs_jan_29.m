clear

t_0 = 0;
t_f = .1;
L = 1;
D = 1;
M = 16;
N = 8;

dx = L/N;
dt = (t_f-t_0)/M;
r = D*dt/dx^2
tic
[u1,err1,x1,t1] = subopt_bcs_heat_eul_neu(t_0,t_f,D,L,M,N);
rtime(1)=toc;

M = 4*M;
N = 2*N;
dx = L/N;
dt = (t_f-t_0)/M;
r = D*dt/dx^2
tic
[u2,err2,x2,t2] = subopt_bcs_heat_eul_neu(t_0,t_f,D,L,M,N);
rtime(2) = toc;

M = 4*M;
N = 2*N;
dx = L/N;
dt = (t_f-t_0)/M;
r = D*dt/dx^2
tic
[u3,err3,x3,t3] = subopt_bcs_heat_eul_neu(t_0,t_f,D,L,M,N);
rtime(3)=toc;


M = 4*M;
N = 2*N;
dx = L/N;
dt = (t_f-t_0)/M;
r = D*dt/dx^2
tic
[u4,err4,x4,t4] = subopt_bcs_heat_eul_neu(t_0,t_f,D,L,M,N);
rtime(4)=toc;

M = 4*M;
N = 2*N;
dx = L/N;
dt = (t_f-t_0)/M;
r = D*dt/dx^2
tic
[u5,err5,x5,t5] = subopt_bcs_heat_eul_neu(t_0,t_f,D,L,M,N);
rtime(5)=toc;

% M = 4*M;
% N = 2*N;
% dx = L/N;
% dt = (t_f-t_0)/M;
% r = D*dt/dx^2
% tic
% [u6,err6,x6,t6] = subopt_bcs_heat_eul_neu(t_0,t_f,D,L,M,N);
% rtime(6)=toc;

size(t1)
size(max(abs(err1)))


E1 = max(abs(err1));
E2 = max(abs(err2));
E3 = max(abs(err3));
E4 = max(abs(err4));
E5 = max(abs(err5));
% E6 = max(abs(err6));

figure(1)
clf
plot(t1,E1,':','LineWidth',2);
hold on
plot(t2,E2,'--','LineWidth',2);
plot(t3,E3,'-.','LineWidth',2);
plot(t4,E4,'-','LineWidth',2);
plot(t5,E5,'r-','LineWidth',2);


figure(2)
clf
plot(t1,log10(E1),':','LineWidth',2);
hold on
plot(t2,log10(E2),'--','LineWidth',2);
plot(t3,log10(E3),'-.','LineWidth',2);
plot(t4,log10(E4),'-','LineWidth',2);
plot(t5,log10(E5),'r-','LineWidth',2);

size(t1)
size(t2)
I = 1:4:length(t2);
size(I)
rat1 = E1./E2(I);

I = 1:4:length(t3);
rat2 = E2./E3(I);

I = 1:4:length(t4);
rat3 = E3./E4(I);

I = 1:4:length(t5);
rat4 = E4./E5(I);

figure(3)
clf
plot(t1,rat1,':','LineWidth',2)
hold on
plot(t2,rat2,'--','LineWidth',2)
plot(t3,rat3,'-.','LineWidth',2);
plot(t4,rat4,'-','LineWidth',2)

I1 = 1:2:length(x2);
J1 = 1:4:length(t2);
I2 = 1:4:length(x3);
J2 = 1:16:length(t3);
diff1 = u1-u2(I1,J1);
diff2 = u2(I1,J1)-u3(I2,J2);
Rat1 = diff1./diff2;
r1 = max(abs(diff1))./max(abs(diff2));

I1 = 1:2:length(x3);
J1 = 1:4:length(t3);
I2 = 1:4:length(x4);
J2 = 1:16:length(t4);
diff1 = u2-u3(I1,J1);
diff2 = u3(I1,J1)-u4(I2,J2);
Rat2 = diff1./diff2;
r2 = max(abs(diff1))./max(abs(diff2));

I1 = 1:2:length(x4);
J1 = 1:4:length(t4);
I2 = 1:4:length(x5);
J2 = 1:16:length(t5);
diff1 = u3-u4(I1,J1);
diff2 = u4(I1,J1)-u5(I2,J2);
Rat3 = diff1./diff2;
r3 = max(abs(diff1))./max(abs(diff2));

dx(1) = x1(2)-x1(1);
dx(2) = x2(2)-x2(1);
dx(3)=dx(2)/2;
dx(4)=dx(3)/2;
dx(5)=dx(4)/2;
%dx(6)=dx(5)/2;
figure(4)
clf
plot(log10(dx),log10(rtime),'o-')  
%X = log10(dx(2:6));
%Y = log10(rtime(2:6));
X = log10(dx(2:5));
Y = log10(rtime(2:5));
clear A
A(:,1)=X;
A(:,2)= ones(size(X));
B = A'*Y';
A = A'*A;
mm = inv(A)*B
hold on
plot(X,mm(1)*X+mm(2),'r')


E(1) = E1(17);
E(2)=E2(65);
E(3)=E3(257);
E(4)=E4(1025);
E(5)=E5(4097);
%E(6)=E6(16385);
figure(5)
clf
plot(log10(dx),log10(E),'o-')
hold on
% X = log10(dx(2:6));
% Y = log10(E(2:6));
X = log10(dx(2:5));
Y = log10(E(2:5));
clear A
A(:,1)=X;
A(:,2)= ones(size(X));
B = A'*Y';
A = A'*A;
mm = inv(A)*B
hold on
plot(X,mm(1)*X+mm(2),'r')

% 
% save subopt_bcs_heat_eul.mat


