% newtn - Program to solve a system of nonlinear equations 
% using Newton's method.  Equations defined by function fnewt.
clear all;  help newtn;  % Clear memory and print header

%* Set initial guess and parameters
x0 = input('Enter the initial guess (row vector): ');
x = x0;  % Copy initial guess
xp(:,1) = x(:); % Record initial guess for plotting
a = input('Enter the parameter a: ');

%* Loop over desired number of steps 
nStep = 10;   % Number of iterations before stopping
for iStep=1:nStep
	
  %* Evaluate function f and its Jacobian matrix D
  [f D] = fnewt(x,a);      % fnewt returns value of f and D
  %* Find dx by Gaussian elimination
  dx = f/D; 
  %* Update the estimate for the root  
  x = x - dx;              % Newton iteration for new x
  xp(:,iStep+1) = x(:); % Save current estimate for plotting
  
end

%* Print the final estimate for the root
fprintf('After %g iterations the root is\n',nStep);
disp(x);

%* Plot the iterations from initial guess to final estimate
figure(1); clf;  % Clear figure 1 window and bring forward
subplot(1,2,1) % Left plot
  plot3(xp(1,:),xp(2,:),xp(3,:),'o-',...
      x(1),x(2),x(3),'*');
  xlabel('x');  ylabel('y'); zlabel('z');
  view([-37.5, 30]);  % Viewing angle
  title(sprintf('Initial guess is %g  %g  %g',x0(1),x0(2),x0(3)));
  grid; drawnow;
subplot(1,2,2) % Right plot
  plot3(xp(1,:),xp(2,:),xp(3,:),'o-',...
      x(1),x(2),x(3),'*');
  xlabel('x');  ylabel('y'); zlabel('z');
  view([-127.5, 30]);  % Viewing angle
  title(sprintf('After %g iterations, root is %g  %g  %g',...
                                       nStep,x(1),x(2),x(3)));
  grid; drawnow;
% Plot data from lorenz (if available). To write lorenz data, use:
% >>save xplot; save yplot; save zplot;
% after running the lornez program.
flag = input('Plot data from lorenz program? (1=Yes/0=No): ');
if( flag == 1 )
  figure(2); clf;  % Clear figure 1 window and bring forward
  load xplot; load yplot; load zplot;
  plot3(xplot,yplot,zplot,'-',xp(1,:),xp(2,:),xp(3,:),'o--');
  xlabel('x'); ylabel('y'); zlabel('z');
  view([40 10]);  % Rotate to get a better view 
  grid;           % Add a grid to aid perspective
end