function [x,y] = swirl_III
% Import solution of swirl_III

temp = load('swirl_III.dat');
x = temp(:,1);
y = temp(:,2:end);
