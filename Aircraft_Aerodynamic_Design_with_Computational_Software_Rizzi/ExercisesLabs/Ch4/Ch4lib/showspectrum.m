%close all
%clear all
load jacfile
lamda = eig(jacw);
figure(1)
plot(lamda, 'ok')
axis equal
xlabel('Re')
ylabel('Im')