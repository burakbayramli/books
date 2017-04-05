function [dx] = dx_forward(u)

dx = [u(:,2:end) u(:,end)] - u;
