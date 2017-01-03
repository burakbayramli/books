% computes vanishing point of two parallel lines in the image plane
% lines are specified by pairs of points l1 = x1 + lambda*(x2-x1);
function [point] = vanishing_point(l1,l2);

x1 = l1(:,1);
x2 = l1(:,2);
x1p = l2(:,1);
x2p = l2(:,2);

A = [x2 - x1, -(x2p - x1p)];
b = [x1p - x1];
if det(A) ~= 0 
  lambda = inv(A)*b;
else 
  error('vanishing point does not exist');
end;

l(1) = line([x1(1) x1(1) + lambda(1)*(x2(1) - x1(1))], ...
             [x1(2) x1(2) + lambda(1)*(x2(2) - x1(2))]);
l(2) = line([x1p(1) x1p(1) + lambda(2)*(x2p(1) - x1p(1))], ...
             [x1p(2) x1p(2) + lambda(2)*(x2p(2) - x1p(2))]);
set(l(1),'Color','red')
set(l(2),'Color','red')

point = x1 + lambda(1)*(x2-x1);



