N = 10;
x1 = 0:1/N:1;
x1 = x1+(1/(2*pi)-.01)*sin(2*pi*x1);
u = alt_linear_elements(x1);
[phi,X1] = linear_building_blocks(x1);
figure(1)
clf
for j=1:N
  plot(X1,phi(j,:));
  hold on
end
figure(1)

U1 = u(2:N+1)*phi;
U = -(9/28)*nthroot((X1-1/2).^7,3)+(3/16)*2^(2/3)*X1-(9/224)*2^(2/3);
figure(2)
clf
plot(x1,u,'-o')
hold on
plot(X1,U1,'x')
plot(X1,U,'r')
figure(2)

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
x2 = 0:1/N:1;
x2 = x2+(1/(2*pi)-.01)*sin(2*pi*x2);
u = alt_linear_elements(x2);
[phi,X2] = linear_building_blocks(x2);
U2 = u(2:N+1)*phi;
U = -(9/28)*(X2-1/2).^(7/3)+(3/16)*2^(2/3)*X2-(9/224)*2^(2/3);
err2 = U2-U;

N = 2*N;
x3 = 0:1/N:1;
x3 = x3+(1/(2*pi)-.01)*sin(2*pi*x3);
u = alt_linear_elements(x3);
[phi,X3] = linear_building_blocks(x3);
U3 = u(2:N+1)*phi;
U = -(9/28)*(X3-1/2).^(7/3)+(3/16)*2^(2/3)*X3-(9/224)*2^(2/3);
err3 = U3-U;

N = 2*N;
x4 = 0:1/N:1;
x4 = x4+(1/(2*pi)-.01)*sin(2*pi*x4);
u = alt_linear_elements(x4);
[phi,X4] = linear_building_blocks(x4);
U4 = u(2:N+1)*phi;
U = -(9/28)*(X4-1/2).^(7/3)+(3/16)*2^(2/3)*X4-(9/224)*2^(2/3);
err4 = U4-U;

N = 2*N;
x5 = 0:1/N:1;
x5 = x5+(1/(2*pi)-.01)*sin(2*pi*x5);
u = alt_linear_elements(x5);
[phi,X5] = linear_building_blocks(x5);
U5 = u(2:N+1)*phi;
U = -(9/28)*(X5-1/2).^(7/3)+(3/16)*2^(2/3)*X5-(9/224)*2^(2/3);
err5 = U5-U;

N = 2*N;
x6 = 0:1/N:1;
x6 = x6+(1/(2*pi)-.01)*sin(2*pi*x6);
u = alt_linear_elements(x6);
[phi,X6] = linear_building_blocks(x6);
U6 = u(2:N+1)*phi;
U = -(9/28)*(X6-1/2).^(7/3)+(3/16)*2^(2/3)*X6-(9/224)*2^(2/3);
err6 = U6-U;

N = 2*N;
x7 = 0:1/N:1;
x7 = x7+(1/(2*pi)-.01)*sin(2*pi*x7);
u = alt_linear_elements(x7);
[phi,X7] = linear_building_blocks(x7);
U7 = u(2:N+1)*phi;
U = -(9/28)*(X7-1/2).^(7/3)+(3/16)*2^(2/3)*X7-(9/224)*2^(2/3);
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



