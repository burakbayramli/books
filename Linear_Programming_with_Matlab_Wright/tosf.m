function [H,h,g] = tosf(A,b,p,maxmin,cons,vars)
% syntax: [H,h,g] = tosf(A,b,p,maxmin,cons,vars)
% input: A,b,p problem data, maxmin = +1 max, -1 min,
%        cons = 1 >=, 0 =, -1 <=; vars = 1 >=0, 0 free, -1 <=0 
% convert to standard form min g'x st Hx >= h, x >= 0

[m,n] = size(A);  
if (m~=length(b) | n~=length(p) | m~=length(cons) | n~=length(vars))
  error('input data is not appropriately dimensioned');
end
vars = vars(:); cons = cons(:);

free = find(vars == 0); nonpos = find(vars < 0);
A(:,nonpos) = -A(:,nonpos); p(nonpos) = -p(nonpos);
noneg = find(vars ~= 0);

equal = find(cons == 0); less = find(cons < 0);
A(less,:) = -A(less,:); b(less) = -b(less);
great = find(cons ~= 0); 

e_f = ones(size(free)); e_e = ones(size(equal))';

h = [b(great); b(equal); -e_e*b(equal)];

H = [ A(great,noneg) A(great,free) -A(great,free)*e_f;
      A(equal,noneg) A(equal,free) -A(equal,free)*e_f; 
      -e_e*A(equal,noneg) -e_e*A(equal,free) e_e*A(equal,free)*e_f]; 

g = -maxmin*[p(noneg); p(free); -p(free)'*e_f]; 

return;
