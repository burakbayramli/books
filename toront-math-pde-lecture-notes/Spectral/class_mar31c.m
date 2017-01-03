

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% explicit timestepping

clear
t_0 = 0; t_f = 1; M = 200; N = 64;
h = 2*pi/N;
x = 0:h:2*pi-h;

eps = 1;
u0 = 1./(eps^2 + sin(x/2).^2);

% phases = rand(1,10)*2*pi;
% amps = rand(1,10);
% nn = length(amps);
% u0 = zeros(size(x(1:N)));
% for j=1:nn
%   u0 = u0 + amps(j)*cos((j-1)*x(1:N)+phases(j));
% end

[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);

mm = max(max(abs(u)));

for j=1:M+1
plot(x,u(:,j))
axis([0,2*pi,-mm-.2,mm+.2])
pause(1)
figure(1)
end
 
for j=1:M+1
plot(kk,amp(:,j),'o-')
axis([0,N/2,-16,2])
figure(1)
pause(1)

end

Err(1) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);
Err(2) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);
Err(3) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);
Err(4) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);
Err(5) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);
Err(6) = max(abs(err(:,M+1)));
% M = 2*M;
% [u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);
% Err(7) = max(abs(err(:,M+1)));

Err
Err(1:5)./Err(2:6)

%%%%%%%%%%%%%%%%%%%  what if time steps are too large?

t_0 = 0; t_f = 1; M = 70; N = 64;32
[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);

mm = max(max(abs(u)));

for j=1:M
plot(x,u(:,j))
figure(1)
pause(1)
end
 
for j=1:M
plot(kk,amp(:,j),'o-')
figure(1)
axis([0,N/2,-16,2])
pause(1)
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% implicit time stepping
% 
% clear
t_0 = 0; t_f = 1; M = 100; N = 64;
% h = 2*pi/N;
% x = 0:h:2*pi;
% 
% phases = rand(1,10)*2*pi;
% amps = rand(1,10);
% nn = length(amps);
% u0 = zeros(size(x(1:N)));
% for j=1:nn
%   u0 = u0 + amps(j)*cos((j-1)*x(1:N)+phases(j));
% end

[u,err,x,t,kk,amp] = heat5(t_0,t_f,M,N,u0);

mm = max(max(abs(u)));

for j=1:M+1
plot(x,u(:,j))
axis([0,2*pi,-mm-.2,mm+.2])
figure(1)
end
 
for j=1:M+1
plot(kk,amp(:,j),'o-')
figure(1)
axis([0,N/2,-16,2])
end

Err(1) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat5(t_0,t_f,M,N,u0);
Err(2) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat5(t_0,t_f,M,N,u0);
Err(3) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat5(t_0,t_f,M,N,u0);
Err(4) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat5(t_0,t_f,M,N,u0);
Err(5) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat5(t_0,t_f,M,N,u0);
Err(6) = max(abs(err(:,M+1)));
% M = 2*M;
% [u,err,x,t,kk,amp] = heat5(t_0,t_f,M,N,u0);
% Err(7) = max(abs(err(:,M+1)))

Err
Err(1:5)./Err(2:6)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Crank-Nicolson time stepping
% 
% clear
t_0 = 0; t_f = 1; M = 100; N = 64;
% h = 2*pi/N;
% x = 0:h:2*pi;
% 
% phases = rand(1,10)*2*pi;
% amps = rand(1,10);
% nn = length(amps);
% u0 = zeros(size(x(1:N)));
% for j=1:nn
%   u0 = u0 + amps(j)*cos((j-1)*x(1:N)+phases(j));
% end

[u,err,x,t,kk,amp] = heat6(t_0,t_f,M,N,u0);

mm = max(max(abs(u)));

for j=1:M+1
plot(x,u(:,j))
axis([0,2*pi,-mm-.2,mm+.2])
figure(1)
end
 
for j=1:M+1
plot(kk,amp(:,j),'o-')
figure(1)
axis([0,N/2,-16,2])
end

Err(1) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat6(t_0,t_f,M,N,u0);
Err(2) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat6(t_0,t_f,M,N,u0);
Err(3) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat6(t_0,t_f,M,N,u0);
Err(4) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat6(t_0,t_f,M,N,u0);
Err(5) = max(abs(err(:,M+1)));
M = 2*M;
[u,err,x,t,kk,amp] = heat6(t_0,t_f,M,N,u0);
Err(6) = max(abs(err(:,M+1)));
% M = 2*M;
% [u,err,x,t,kk,amp] = heat5(t_0,t_f,M,N,u0);
% Err(7) = max(abs(err(:,M+1)))

Err
Err(1:5)./Err(2:6)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exact solver
%
t_0 = 0; t_f = 1; M = 100; N = 32;
h = 2*pi/N;
x = 0:h:2*pi;

phases = rand(1,10)*2*pi;
amps = rand(1,10);
nn = length(amps);
u0 = zeros(size(x(1:N)));
for j=1:nn
  u0 = u0 + amps(j)*cos((j-1)*x(1:N)+phases(j));
end

[u,x,t,kk,amp] = heat7(t_0,t_f,M,N,u0);

mm = max(max(abs(u)));
for j=1:M+1
plot(x,u(:,j))
axis([0,2*pi,-mm-.2,mm+.2])
figure(1)
end
 
for j=1:M+1
plot(kk,amp(:,j),'o-')
axis([0,N/2,-16,2])
figure(1)
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t_0 = 0; t_f = 1; M = 1000; N = 256;
[u,x,t,kk,amp] = burgers(t_0,t_f,M,N);

for j=1:M
plot(x,u(:,j))
axis([0,2*pi,-1.2,1.2])
% pause(1)
figure(1)
end

for j=1:23
plot(x,u(:,(j-1)*30+1))
axis([0,2*pi,-1.2,1.2])
pause(1)
figure(1)
end

 
for j=1:40
plot(kk,amp(:,1+(j-1)*30),'o-')
axis([0,N/2,-16,2])
figure(1)
pause(1)
end
