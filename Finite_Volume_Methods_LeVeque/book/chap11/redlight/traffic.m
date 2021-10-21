
% Traffic flow examples.  Car velocity depends on density seen by driver.
% This looks best if you make the matlab plot window tall and skinny.
% R.J. LeVeque,  January, 1998


% Choose an example and un-comment:

% Example 1
% traffic hitting wall and shock wave:

   wall = 10;
   xlim = [-5 11];
   tlim = [-16 2];
   carpos = -25:1:5;
   umax = 1.5;
   nsteps = 40

% Example 2
% rarefaction wave when the light turns green:

   % wall = 100;
   % xlim = [-10 10];
   % tlim = [-10 2];
   % carpos = -20:.4:0;
   % umax = 1.0;
   % nsteps = 20


% Example 3
% local congestion giving a shock followed by rarefaction:
% (also try this data with velocity = 1 below to see linear advection)

   % wall = 100;
       %carpos = [-40:2:1 1.5:.5:2.5 3:2:12];
       %carpos = [-50:2:-1 -.5:.5:2.5 3:2:32];
   % carpos = [-47:4:-3 -2.5:.5:2.5 3:4:43];
   % xlim = [-20 40];
   % tlim = [-20 2];
   % umax = 1.5;
   % nsteps = 40;

clf
ncar = length(carpos);
carlength = .4;
dt = .4;
tmax = nsteps*dt;
substeps = 4;
dts = dt/substeps;
time = 0;
times = 0;
carpost = carpos;

% data to draw one little car:
xcar0 = [-.2 .2 .2 .05 .05 -.05 -.05 -.2 -.2];
ycar0 = [  0  0 .1 .1  .2   .2   .1   .1   0];


% plot initial data:

density = carlength ./ (carpos(2:ncar) - carpos(1:(ncar-1))); 
density(ncar) = .5*carlength ./ (wall - carpos(ncar));

hcars = axes('position',[.1 .1 .8 .7]);
for i=1:ncar
     % draw cars
     xcar = xcar0 + carpos(i);
     ycar = ycar0 + time;
     plot(xcar,ycar)
     hold on
     end

plot([wall  wall ],tlim,'r')
axis([xlim   tlim])
hold off
  hdensity = axes('position',[.1 .85 .8 .1]);
  plot(carpos, density,'ok')
  title(['density at time ', num2str(time)])
  axis([xlim   -.1 1.1])
pause(3)
%makeframe(1,[400,250])
%makeframe(2,[400,250])
clf

%
% Time-stepping loop:

for n=1:nsteps

  % take several substeps within each plotting step to give better
  % accuracy integrating car paths:

  for nsub = 1:substeps
    time = time + dts;
    times = [times; time];
    %
    % compute density and velocity of each car:
    %
    density = carlength ./ (carpos(2:ncar) - carpos(1:(ncar-1))); 
    density(ncar) = .5*carlength ./ (wall - carpos(ncar));
    %
    % nonlinear equation if velocity depends on density:
    %
    velocity = umax * (1 - density);
    velocity = max(velocity, 0);
    %
    % linear advection for constant velocity:
    % velocity = 1;
    %
    % move each car based on velocity (forward Euler):

    carpos = carpos + dts*velocity;
    carpost = [carpost; carpos];  % array of locations to plot path
    end

  % plot data at time t:
  clf
  hcars = axes('position',[.1 .1 .8 .7]);
  for i=1:ncar
     % draw cars
     xcar = xcar0 + carpos(i);
     ycar = ycar0 ;
     plot(xcar,ycar)
     hold on
     plot(carpost(:,i), times-time, 'red')
     end
  axis([xlim   tlim])
  plot([wall  wall ],tlim,'r')
  hold off
  hdensity = axes('position',[.1 .85 .8 .1]);
  plot([carpos,wall], [density,density(ncar)],'ok')
  plot(carpos, density,'ok')
  title(['density at time ', num2str(time)])
  axis([xlim   -.1 1.1])
  drawnow
  pause(.2)
  % Frame = n;
  % makeframegif
  end;

