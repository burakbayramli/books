class_mar13
clear E1
E1(1,1) = max(abs(err1));
h = X1(2)-X1(1);
E1(1,2) = sqrt(h*sum(err1.^2));

E1(2,1) = max(abs(err2));
h = X2(2)-X2(1);
E1(2,2) = sqrt(h*sum(err2.^2));

E1(3,1) = max(abs(err3));
h = X3(2)-X3(1);
E1(3,2) = sqrt(h*sum(err3.^2));

E1(4,1) = max(abs(err4));
h = X4(2)-X4(1);
E1(4,2) = sqrt(h*sum(err4.^2));

E1(5,1) = max(abs(err5));
h = X5(2)-X5(1);
E1(5,2) = sqrt(h*sum(err5.^2));

E1(6,1) = max(abs(err6));
h = X6(2)-X6(1);
E1(6,2) = sqrt(h*sum(err6.^2));

E1(7,1) = max(abs(err7));
h = X7(2)-X7(1);
E1(7,2) = sqrt(h*sum(err7.^2));

d1(1) = x1(2)-x1(1);
d1(2) = x2(2)-x2(1);
d1(3) = x3(2)-x3(1);
d1(4) = x4(2)-x4(1);
d1(5) = x5(2)-x5(1);
d1(6) = x6(2)-x6(1);
d1(7) = x7(2)-x7(1);

class_mar13_nonuniform
clear E2
E2(1,1) = max(abs(err1));
% use trapezoidal rule to approximate the L^2 norm on nonuniform mesh
N = length(err1)-1;
E2(1,2) = sqrt(sum((diff(X1)/2).*(err1(1:N)+err1(2:N+1)).^2));
E2(2,1) = max(abs(err2));
N = length(err2)-1;
E2(2,2) = sqrt(sum((diff(X2)/2).*(err2(1:N)+err2(2:N+1)).^2));
E2(3,1) = max(abs(err3));
N = length(err3)-1;
E2(3,2) = sqrt(sum((diff(X3)/2).*(err3(1:N)+err3(2:N+1)).^2));
E2(4,1) = max(abs(err4));
N = length(err4)-1;
E2(4,2) = sqrt(sum((diff(X4)/2).*(err4(1:N)+err4(2:N+1)).^2));
E2(5,1) = max(abs(err5));
N = length(err5)-1;
E2(5,2) = sqrt(sum((diff(X5)/2).*(err5(1:N)+err5(2:N+1)).^2));
E2(6,1) = max(abs(err6));
N = length(err6)-1;
E2(6,2) = sqrt(sum((diff(X6)/2).*(err6(1:N)+err6(2:N+1)).^2));
E2(7,1) = max(abs(err7));
N = length(err7)-1;
E2(7,2) = sqrt(sum((diff(X7)/2).*(err7(1:N)+err7(2:N+1)).^2));

d2(1) = max(diff(x1));
d2(2) = max(diff(x2));
d2(3) = max(diff(x3));
d2(4) = max(diff(x4));
d2(5) = max(diff(x5));
d2(6) = max(diff(x6));
d2(7) = max(diff(x7));

d3(1) = min(diff(x1));
d3(2) = min(diff(x2));
d3(3) = min(diff(x3));
d3(4) = min(diff(x4));
d3(5) = min(diff(x5));
d3(6) = min(diff(x6));
d3(7) = min(diff(x7));

figure(1)
clf
plot(log10(d1),log10(E1(:,1)),'o-');
hold on
plot(log10(d2),log10(E2(:,1)),'ro-');
plot(log10(d3),log10(E2(:,1)),'wo-');

figure(2)
clf
plot(log10(d1),log10(E1(:,2)),'o-');
hold on
plot(log10(d2),log10(E2(:,2)),'ro-');
plot(log10(d3),log10(E2(:,2)),'wo-');


