function [x,y] = swave
% Import solution of swave
temp = load('swave.dat');
x = temp(:,1);
y = temp(:,2:end);
plot(x,y(:,1));