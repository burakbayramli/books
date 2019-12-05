function out = lsqdata( )
%A demo to show recursive least squares parameter estimation and prediction.

clf;
numpts = 200;
order = 2;
noise = 1.0;
trsize = 5;
theta = [0.607;0.4];

disp('True parameter vector = ')
disp(theta');

y=10+ noise*(rand(1,order)-0.5);

fudge=0.5;
theta = fudge*theta;
for i=order+1:numpts,
  for j=1:order,
    phip(j) = y(i-j);
  end
  phi=phip';
  if i==trsize+1,
    theta = theta/fudge;
    phi(1)=phi(1)*10/y(i-1);
  end
  y(i)=theta'*phi + noise*(rand(1)-0.5);
end
x=[1:numpts];
plot(x,y,'r-');

disp('Press any key to plot predicted signal');
pause

hold on;

for i=1:order+1,
  ytr(:,i)=y(i:trsize+i-1)';
end

for i=1:order,
  A(:,i)=ytr(:,order-i+1);
end
b = ytr(:,order+1);

thetahat=(A'*A)\(A'*b);

disp('Estimate of parameter vector = ')
disp(thetahat');

for j=1:order,
  yhat(j)=y(j);
end

for i=order+1:numpts,
  for j=1:order,
    phip(j) = y(i-j);
  end
  phi=phip';
  yhat(i)=thetahat'*phi;
end
x=[1:numpts];
plot(x,yhat,'b-.');

disp('Press any key to plot RLS predicted signal');
pause

for i=1:order+1,
  ytr2(:,i)=y(i:order+i-1)';
end

for i=1:order,
  A2(:,i)=ytr2(:,order-i+1);
end
b2 = ytr2(:,order+1);

P=inv(A2'*A2);
that=P*A2'*b2;

for j=1:order,
  yhat(j)=y(j);
end

for i=order+1:numpts,
  for j=1:order,
    phip(j) = y(i-j);
  end
  phi=phip';
  yhat(i)=that'*phi;
  P=P-(P*phi*phi'*P)/(1+phi'*P*phi);
  that=that+P*phi*(y(i)-phi'*that);
  %disp('that =');
  %disp(that');
end

disp('Estimate of parameter vector = ')
disp(that');

x=[1:numpts];
plot(x,yhat,'g-');
