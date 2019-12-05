function out = lsqdata( )
%A demo to show least squares parameter estimation and prediction.

clf;
numpts = 200;
order = 2;
noise = 1.0;
theta = [0.6;0.4];

disp('True parameter vector = ')
disp(theta');

y=10 + noise*(rand(1,order)-0.5);

for i=order+1:numpts,
  for j=1:order,
    phip(j) = y(i-j);
  end
  phi=phip';
  y(i)=theta'*phi + noise*(rand(1)-0.5);
end
x=[1:numpts];
plot(x,y,'r-');

disp('Press any key to plot predicted signal');
pause

hold on;

trsize = floor(numpts/4);

for i=1:order+1,
  ytr(:,i)=y(i:trsize+i-1)';
end

for i=1:order,
  A(:,i)=ytr(:,order-i+1);
end
b = ytr(:,order+1);

%z0=y(1:trsize)';
%z1=y(2:trsize+1)';
%z2=y(3:trsize+2)';
%Y0Y0 = z0'*z0;
%Y1Y1 = z1'*z1;
%Y0Y1 = z0'*z1;
%Y0Y2 = z0'*z2;
%Y1Y2 = z1'*z2;
%thetahat=[Y0Y0*Y1Y2-Y0Y1*Y0Y2; Y1Y1*Y0Y2 - Y0Y1*Y1Y2]/(Y0Y0*Y1Y1-(Y0Y1)^2)

%thetahat=theta;
%thetahat=A\b;
thetahat=(A'*A)\(A'*b);
%thetahat=[1;1];

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

disp('Press any key to plot predicted signal (1.5x values)');
pause

thetahat=1.5*theta;

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
plot(x,yhat,'g-');

disp('Press any key to plot predicted signal (0.5x values)');
pause

thetahat=0.5*theta;

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
plot(x,yhat,'m-');
