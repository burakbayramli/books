%DW     Script for investigation of the effect of
%       change of weight on the solution

%Kai Borre 11-05-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

A = [1 1; 1 2; -1 1];
b = [2; 1; 0];
C = diag([ 1 2 1]);
x = inv(A'*C*A)*A'*C*b;
r = A*x-b;
sol = [];
for deltaC = -1:100     % -1 is the weight that eliminates the obs.
   dx = -inv(A'*C*A + A(3,:)'*deltaC*A(3,:)) * A(3,:)'*deltaC*r(3,1);
   sol = [sol x+dx];
end
clf
figure(1);
hold on
plot([1/3  1  3 1/3],[1/3 1 -1 1/3],'g')
plot(sol(1,:),sol(2,:))
for i = 1:102
   st = num2str(i-2);
   text(sol(1,i)+.02,sol(2,i)+.02,st)
end
zoom
hold off
print -deps dw.eps
deltaC = 10.e10;
dinf = -inv(A'*C*A + A(3,:)'*deltaC*A(3,:)) * A(3,:)'*deltaC*r(3,1);
solinf = x+dinf
%%%%%%%%%%%%%%%%%%% end dw.m %%%%%%%%%%%%%%%%%
