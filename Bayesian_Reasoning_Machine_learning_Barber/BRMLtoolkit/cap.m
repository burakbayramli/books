function x=cap(x,c)
%CAP Cap x at absolute value c
% x=cap(x,c)
x(find(abs(x)>c))=c*sign(x(find(abs(x)>c)));