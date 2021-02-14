function [x,y] = MUSN
% Import solution of MUSN

temp = load('MUSN.dat');
x = temp(:,1);
y = temp(:,2:end);
