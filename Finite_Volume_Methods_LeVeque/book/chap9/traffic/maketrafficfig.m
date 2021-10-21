
% Traffic flow examples.  Car velocity u depends on density seen by driver.
% Set in function speed
% R.J. LeVeque,  June, 1999



ncar = length(X);
carlength = 1;
dts = .1;
substeps = tend/dts;
time = 0;
times = time;
Xt = X;
average = 0;

% data to draw one little car:
xcar0 = 2.5*[-.2 .2 .2 .05 .05 -.05 -.05 -.2 -.2];
ycar0 = 2.5*[  0  0 .1 .1  .2   .2   .1   .1   0];
ycar0 = 1.25 * ycar0 * (tlim(2)-tlim(1))/(xlim(2)-xlim(1));


% plot initial data:

density = carlength ./ (X(2:ncar) - X(1:(ncar-1))); 
density(ncar) = .5*carlength ./ (wall - X(ncar));

% average density for example 6 (nonconvex):
if average==1
 for i=ncar:-1:2
    density(i) = 0.5*(density(i) + density(i-1));
    end
 end

figure(1)
clf
hcars = axes('position',[.1 .1 .8 .8]);
for i=1:ncar
     % draw cars
     xcar = xcar0 + X(i);
     ycar = ycar0 + time;
     plot(xcar,ycar,'r')
     hold on
     %fill(xcar,ycar,'b')
     end


plot([wall  wall ],tlim,'r')
axis([xlim   tlim])

figure(2)
clf
  hdensity = axes('position',[.1 .1 .8 .35]);
  plot(X, density,'ok')
  title(['density at time ', num2str(time)])
  axis([xlim   -.1 1.1])
query

%
% Time-stepping loop:

for n=1:nsteps

  % take several substeps within each plotting step to give better
  % accuracy integrating car paths:

  for nsub = 1:substeps
    time = time + dts;
    times = [times; time];
    %
    % compute density and speed of each car:
    %

    density = carlength ./ (X(2:ncar) - X(1:(ncar-1))); 
    density(ncar) = .5*carlength ./ (wall - X(ncar));
    
    if average==1
     for i=ncar:-1:2
        density(i) = 0.5*(density(i) + density(i-1));
        end
     end

    u = feval(speed,density,X);
    u = max(u, 0);
     
    % move each car based on u (forward Euler):

    X = X + dts*u;
    Xt = [Xt; X];  % array of locations to plot path
    end

  % plot data at time t:
  figure(1)
  hold on
  %hcars = axes('position',[.1 .1 .8 .4]);
  for i=1:ncar
     % draw cars
     xcar = xcar0 + X(i);
     % ycar = ycar0 ;
     ycar = ycar0 + time ;
     plot(xcar,ycar,'r')
     hold on
     %fill(xcar,ycar,'b')
     plot(Xt(:,i), times, 'blue')
     if exno<0
       ycar = ycar0 + time;
       plot(Xt(:,i), times, 'blue')
       end
     end
  axis([xlim   tlim])
  plot([wall  wall ],tlim,'r')

  figure(3)
  clf
  hdensity = axes('position',[.1 .1 .8 .35]);
  plot([X,wall], [density,density(ncar)],'ok')
  plot(X, density,'ok')
  title(['density at time ', num2str(time)])
  axis([xlim   -.1 1.1])
  pause(.2)
  if exno<0
     query
     end
  end;


figure(1)
if exno==4
   % add lines to show slow zone:
   plot([10 10],[-2 80],'r--')
   plot([20 20],[-2 80],'r--')
   end
if abs(exno)==5
   plot([0 0],[-2 80],'r--')
   end

eval(['print ex',num2str(exno),'cars -deps'])
figure(2)
eval(['print ex',num2str(exno),'rho0 -deps'])
figure(3)
eval(['print ex',num2str(exno),'rho1 -deps'])
