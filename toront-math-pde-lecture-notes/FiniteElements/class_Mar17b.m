N = 10;
[x1,u] = quartic_elements(N);
[phi,X1] = quartic_building_blocks(x1);
figure(1)
clf
for j=1:N
  plot(X1,phi(j,:));
  hold on
end
figure(1)

U1 = u(2:N+1)*phi;
U = -(1/56)*X1.^8+(1/7)*X1;
figure(2)
clf
plot(x1,u,'-o')
hold on
plot(X1,U1,'x')
plot(X1,U,'ro')
figure(2)

figure(3)
clf
plot(X1,U1./U)

err1 = U1-U;
figure(3)
clf
plot(X1,abs(err1));
figure(3)

figure(4)
clf
plot(X1,log10(abs(err1)));
figure(4)

N = 2*N;
[x2,u] = quartic_elements(N);
[phi,X2] = quartic_building_blocks(x2);
U2 = u(2:N+1)*phi;
U = -(1/56)*X2.^8+(1/7)*X2;
err2 = U2-U;

N = 2*N;
[x3,u] = quartic_elements(N);
[phi,X3] = quartic_building_blocks(x3);
U3 = u(2:N+1)*phi;
U = -(1/56)*X3.^8+(1/7)*X3;
err3 = U3-U;

N = 2*N;
[x4,u] = quartic_elements(N);
[phi,X4] = quartic_building_blocks(x4);
U4 = u(2:N+1)*phi;
U = -(1/56)*X4.^8+(1/7)*X4;
err4 = U4-U;

N = 2*N;
[x5,u] = quartic_elements(N);
[phi,X5] = quartic_building_blocks(x5);
U5 = u(2:N+1)*phi;
U = -(1/56)*X5.^8+(1/7)*X5;
err5 = U5-U;

N = 2*N;
[x6,u] = quartic_elements(N);
[phi,X6] = quartic_building_blocks(x6);
U6 = u(2:N+1)*phi;
U = -(1/56)*X6.^8+(1/7)*X6;
err6 = U6-U;

N = 2*N;
[x7,u] = quartic_elements(N);
[phi,X7] = quartic_building_blocks(x7);
U7 = u(2:N+1)*phi;
U = -(1/56)*X7.^8+(1/7)*X7;
err7 = U7-U;


figure(1)
clf
plot(X1,U1);
hold on
plot(X2,U2);
plot(X3,U3);
plot(X4,U4);
plot(X5,U5);
plot(X6,U6);
plot(X7,U7);
plot(X7,U,'r')
figure(1)

format short e
display('errors')
max(abs(err1))
max(abs(err2))
max(abs(err3))
max(abs(err4))
max(abs(err5))
max(abs(err6))
max(abs(err7))
display(' ')

display('ratios of errors')

max(abs(err1))/max(abs(err2))
max(abs(err2))/max(abs(err3))
max(abs(err3))/max(abs(err4))
max(abs(err4))/max(abs(err5))
max(abs(err5))/max(abs(err6))
max(abs(err6))/max(abs(err7))



