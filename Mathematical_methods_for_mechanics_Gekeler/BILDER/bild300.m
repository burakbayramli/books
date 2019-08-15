% Skizze zu aufgabe 23
figure
set(gcf,'Renderer','zbuffer');
T = linspace(0,2*pi,30);
N = length(T);
X = cos(T);
Y = sin(T);
U= -1*ones(1,N);
V= 1*ones(1,N);
plot3(X,Y,U);
hold on
plot3(X,Y,V);
hold on
for I = 1:N-1
A = [X(I), X(I+1), X(I+1), X(I)];
B = [Y(I), Y(I+1), Y(I+1), Y(I)];
C = [U(I), U(I)  , V(I)  , V(I)];
fill3(A,B,C,'g')
hold on
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
T = linspace(-pi/2,pi/2,15);
N = length(T);
X = cos(T) - 1.5;
Y = sin(T);
U= -1*ones(1,N);
V= 1*ones(1,N);
plot3(X,Y,U);
hold on
plot3(X,Y,V);
hold on
for I = 1:N-1
A = [X(I), X(I+1), X(I+1), X(I)];
B = [Y(I), Y(I+1), Y(I+1), Y(I)];
C = [U(I), U(I)  , V(I)  , V(I)];
fill3(A,B,C,'c')
hold on
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

T = linspace(pi/2,3*pi/2,15);
N = length(T);
X = cos(T) + 1.5;
Y = sin(T);
U= -1*ones(1,N);
V= 1*ones(1,N);
plot3(X,Y,U);
hold on
plot3(X,Y,V);
hold on
for I = 1:N-1
A = [X(I), X(I+1), X(I+1), X(I)];
B = [Y(I), Y(I+1), Y(I+1), Y(I)];
C = [U(I), U(I)  , V(I)  , V(I)];
fill3(A,B,C,'y')
hold on
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a = 1.5
A = [0,  0,  0,  0, 0];
B = [1, -1, -1,  1, 1]*a;
C = [1,  1, -1, -1, 1]*a;
plot3(A,B,C)
hold on
fill3(A,B,C,'r')
hold on
A = [-1,  1, 1, -1,-1];
B = [-1, -1, 1,  1,-1]*a;
C = [0,  0,  0,  0, 0]*a;
plot3(A,B,C)
hold on
fill3(A,B,C,'b')

%plot3(A(1),B(1),C(1),'.','MarkerSize',6)
%hold on
%plot3(A(2),B(2),C(2),'.','MarkerSize',6)
%hold on
xlabel('x-Achse')
ylabel('y-Achse')
grid on
