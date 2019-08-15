function ch3ex8
sol = bvpinit(linspace(0,1,5),[1 1 1 1]);
n = 5e-2;
lambda = 2;
fprintf(' kappa    computed Os  approximate Os \n')
for kappa = 2:5
  eta = lambda^2/(n*kappa^2);
  sol = bvp4c(@odes,@bcs,sol,[],n,lambda,eta);
  K2 = lambda*sinh(kappa/lambda)/(kappa*cosh(kappa));
  approx = 1/(1 - K2);
  computed = 1/sol.y(3,end);
  fprintf('  %2i    %10.3f    %10.3f \n',kappa,computed,approx);
end
% v and C are computed separately on 0 <= x <= 1 and 1 <= x <= lambda.
% A change of independent variable is used for the second interval.
% First it must be undone to obtain the corresponding mesh in x and
% then a solution assembled for all of 0 <= x <= lambda.
x = [sol.x sol.x*(lambda-1)+1];
y = [sol.y(1:2,:) sol.y(3:4,:)];
plot(x,y(1,:),x,y(2,:))
legend('v(x)','C(x)')
%print -depsc ch3fig8
%=================================================
function dydx = odes(x,y,n,lambda,eta)
dydx = [ (y(2) - 1)/n
         (y(1)*y(2) - x)/eta 
         (lambda - 1)*(y(4) - 1)/n
         (lambda - 1)*(y(3)*y(4) - 1)/eta ];

function res = bcs(ya,yb,n,lambda,eta)
res = [ ya(1); yb(4)-1; yb(1)-ya(3); yb(2)-ya(4)];