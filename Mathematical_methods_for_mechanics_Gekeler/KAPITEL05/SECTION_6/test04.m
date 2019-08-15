function test04
% Solving the algebraic bifurcation equation
clc, clf
%B = I;
rand('state',sum(100*clock));
%B = rand(2,2); C1 = rand(2,2); C2 = rand(2,2);
%save datentst B C1 C2
load datentst B C1 C2

P = [3,0;0,1]; Q = [1/2, 0; 0, 4];
n = 200;
tt = linspace(0,2*pi,n+1);
X = cos(tt); Y = sin(tt); XX = [X;Y];
Z = []; mu = 0.5;
for I = 1:length(tt)
F =  mu*B*XX(:,I) + [XX(:,I).'*P*XX(:,I);XX(:,I).'*Q*XX(:,I)];
Z = [Z,norm(F)];
end
plot(Z,'r'), hold on
Z = []; mu = 3;
for I = 1:length(tt)
F =  mu*B*XX(:,I) + [XX(:,I).'*P*XX(:,I);XX(:,I).'*Q*XX(:,I)];
Z = [Z,norm(F)];
end
plot(Z,'g'), hold on

    