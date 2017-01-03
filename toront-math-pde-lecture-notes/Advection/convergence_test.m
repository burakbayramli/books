
t_0 = 0; t_f = 1; N = 10; M = 10;
    
for j=1:9
    [u,u_exact,x,t] = explicit_upwind(@f,t_0,t_f,M,N);
    err_inf(j) = max(abs(u(:,M+1)-u_exact(:,M+1)));
    err_one(j) = sum((x(2)-x(1))*abs(u(:,M+1)-u_exact(:,M+1)));
    err_two(j) = sqrt(sum((x(2)-x(1))*(u(:,M+1)-u_exact(:,M+1)).^2));
    Dt(j) = (t_f-t_0)/M;
    Dx(j) = 2*pi/N;
    N = 2*N; M = 2*M;
end
err_inf
err_two
err_one
display('Explicit Upwind ratios')
err_inf(1:8)./err_inf(2:9)
err_two(1:8)./err_two(2:9)
err_one(1:8)./err_one(2:9)
figure(1)
clf
plot(log10(Dx),log10(err_inf));
hold on
plot(log10(Dt),log10(err_inf));
plot(log10(Dx),log10(err_one),'r');
plot(log10(Dt),log10(err_one),'r');
plot(log10(Dx),log10(err_two),'b');
plot(log10(Dt),log10(err_two),'b');
hold off

t_0 = 0; t_f = 1; N = 10; M = 10;
    
for j=1:9
    [u,u_exact,x,t] = lax_friedrichs(@f,t_0,t_f,M,N);
    err_inf(j) = max(abs(u(:,M+1)-u_exact(:,M+1)));
    err_one(j) = sum((x(2)-x(1))*abs(u(:,M+1)-u_exact(:,M+1)));
    err_two(j) = sqrt(sum((x(2)-x(1))*(u(:,M+1)-u_exact(:,M+1)).^2));
    Dt(j) = (t_f-t_0)/M;
    Dx(j) = 2*pi/N;
    N = 2*N; M = 2*M;
end
err_inf
err_two
err_one
display('Lax Friedrichs ratios')
err_inf(1:8)./err_inf(2:9)
err_two(1:8)./err_two(2:9)
err_one(1:8)./err_one(2:9)
figure(2)
clf
plot(log10(Dx),log10(err_inf));
hold on
plot(log10(Dt),log10(err_inf));
plot(log10(Dx),log10(err_one),'r');
plot(log10(Dt),log10(err_one),'r');
plot(log10(Dx),log10(err_two),'b');
plot(log10(Dt),log10(err_two),'b');
hold off

t_0 = 0; t_f = 1; N = 10; M = 10;
    
for j=1:9
    [u,u_exact,x,t] = lax_wendroff(@f,t_0,t_f,M,N);
    err_inf(j) = max(abs(u(:,M+1)-u_exact(:,M+1)));
    err_one(j) = sum((x(2)-x(1))*abs(u(:,M+1)-u_exact(:,M+1)));
    err_two(j) = sqrt(sum((x(2)-x(1))*(u(:,M+1)-u_exact(:,M+1)).^2));
    Dt(j) = (t_f-t_0)/M;
    Dx(j) = 2*pi/N;
    N = 2*N; M = 2*M;
end
err_inf
err_two
err_one
display('Lax Wendroff ratios')
err_inf(1:8)./err_inf(2:9)
err_two(1:8)./err_two(2:9)
err_one(1:8)./err_one(2:9)
figure(3)
clf
plot(log10(Dx),log10(err_inf));
hold on
plot(log10(Dt),log10(err_inf));
plot(log10(Dx),log10(err_one),'r');
plot(log10(Dt),log10(err_one),'r');
plot(log10(Dx),log10(err_two),'b');
plot(log10(Dt),log10(err_two),'b');
hold off

t_0 = 0; t_f = 1; N = 10; M = 10;
    
for j=1:9
    [u,u_exact,x,t] = beam_warming(@f,t_0,t_f,M,N);
    err_inf(j) = max(abs(u(:,M+1)-u_exact(:,M+1)));
    err_one(j) = sum((x(2)-x(1))*abs(u(:,M+1)-u_exact(:,M+1)));
    err_two(j) = sqrt(sum((x(2)-x(1))*(u(:,M+1)-u_exact(:,M+1)).^2));
    Dt(j) = (t_f-t_0)/M;
    Dx(j) = 2*pi/N;
    N = 2*N; M = 2*M;
end
err_inf
err_two
err_one
display('Beam Warming ratios')
err_inf(1:8)./err_inf(2:9)
err_two(1:8)./err_two(2:9)
err_one(1:8)./err_one(2:9)
figure(4)
clf
plot(log10(Dx),log10(err_inf));
hold on
plot(log10(Dt),log10(err_inf));
plot(log10(Dx),log10(err_one),'r');
plot(log10(Dt),log10(err_one),'r');
plot(log10(Dx),log10(err_two),'b');
plot(log10(Dt),log10(err_two),'b');
hold off

