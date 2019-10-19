function [x] = fromsf(y,vars)
% syntax: [x] = fromsf(y,vars)
% input: y is solution of sf problem; vars = 1 >=0, 0 free -1 <= 0
% convert y back to original problem form

free = find(vars==0); noneg = find(vars ~= 0);
f = length(free); n = length(noneg); 

x = zeros(n+f,1);
if n>0
  x(noneg) = vars(noneg)'.*y(1:n);
end
if f>0
  x(free) = y(n+1:n+f) - y(n+f+1)*ones(f,1);
end

return;
