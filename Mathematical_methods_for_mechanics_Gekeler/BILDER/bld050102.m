function bld050102
% Cusp Katastrophe
% z^3 + y*z + x = 0
clf
faktor = 1;
N = 40;
a = -1.5;
b = 0.5;
set(gcf,'renderer','zbuffer')
plot3(0,0,0,'.','markersize',6), hold on
x = 1; y = -1;
p = [1, 0, x, y];
r = roots(p);
E1 = [a;b;r(3)];
plot3(E1(1),E1(2),E1(3),'markersize',6), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X1 = ones(1,N);
Y1 = linspace(a,b,N);
Z1 = zeros(1,N);
for I=1:N
   p = [1,0,Y1(I),X1(I)];
   r = roots(p);
   J = find(imag(r) == 0);
   Z1(I) = r(J);
end
Z1 = faktor*Z1;
plot3(X1,Y1,Z1,'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X2 = linspace(-1,1,N);
Y2 = b*ones(1,N);
Z2 = zeros(1,N);
for I=1:N
   p = [1,0,Y2(I),X2(I)];
   r = roots(p);
   J = find(imag(r) == 0);
   Z2(I) = r(J);
end
Z2 = faktor*Z2;
plot3(X2,Y2,Z2,'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X3 = -ones(1,N);
Y3 = linspace(a,b,N);
Z3 = zeros(1,N);
for I=1:N
   p = [1,0,Y3(I),X3(I)];
   r = roots(p);
   J = find(imag(r) == 0);
   Z3(I) = r(J);
end
Z3 = faktor*Z3;
plot3(X3,Y3,Z3,'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Z4 = linspace(Z3(1),Z1(1),N);
Y4 = a*ones(1,N);
X4 =  - Z4.*Z4.*Z4 - Y4.*Z4;
plot3(X4,Y4,Z4,'linewidth',2,'color','k'), hold on
%-- Falte 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y5 = linspace(a,0,N);
X5 = real(2*(-Y5/3).^(3/2));
Z5 = real(sqrt(-Y5/3));
plot3(X5,Y5,Z5,'linewidth',2,'color','k'), hold on
%-- Falte 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
M = 13;
Y6 = linspace(a,0,N);
Y7 = Y6(1:M);
X7 = -real(2*(-Y7/3).^(3/2));
Z7 = -real(sqrt(-Y7/3));
plot3(X7,Y7,Z7,'linewidth',2,'color','k'), hold on
Y8 = Y6(M+1:N);
X8 = -real(2*(-Y8/3).^(3/2));
Z8 = -real(sqrt(-Y8/3));
plot3(X8,Y8,Z8,'k--','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
view(30,60)
xlabel('\lambda','fontsize',22)
ylabel('\mu','fontsize',22)
zlabel('x','fontsize',22)
grid on
