function [x,v,strikes,delv] = mover(x,v,npart, ...
                                L,mpv,vwall,tau)
% mover - Function to move particles by free flight
%         Also handles collisions with walls
% Inputs
%    x        Positions of the particles
%    v        Velocities of the particles
%    npart    Number of particles in the system
%    L        System length
%    mpv      Most probable velocity off the wall
%    vwall    Wall velocities
%    tau      Time step
% Outputs
%    x,v      Updated positions and velocities
%    strikes  Number of particles striking each wall
%    delv     Change of y-velocity at each wall     

%* Move all particles pretending walls are absent
x_old = x;            % Remember original position
x(:) = x_old(:) + v(:,1)*tau;  

%* Loop over all particles
strikes = [0 0];  delv = [0 0];  
xwall = [0 L];  vw = [-vwall vwall];
direction = [1 -1];   % Direction of particle leaving wall
stdev = mpv/sqrt(2);
for i=1:npart

  %* Test if particle strikes either wall
  if( x(i) <= 0 )
    flag=1; % Particle strikes left wall
  elseif( x(i) >= L )
    flag=2; % Particle strikes right wall
  else
    flag=0;  % Particle strikes neither wall
  end

  %* If particle strikes a wall, reset its position
  %  and velocity. Record velocity change.
  if( flag > 0 )
    strikes(flag) = strikes(flag) + 1;
    vyInitial = v(i,2);
    %* Reset velocity components as biased Maxwellian,
    %  Exponential dist. in x; Gaussian in y and z
    v(i,1) = direction(flag)*sqrt(-log(1-rand(1))) * mpv;
    v(i,2) = stdev*randn(1) + vw(flag); % Add wall velocity
    v(i,3) = stdev*randn(1);
    % Time of flight after leaving wall
    dtr = tau*(x(i)-xwall(flag))/(x(i)-x_old(i));   
    %* Reset position after leaving wall
    x(i) = xwall(flag) + v(i,1)*dtr;
    %* Record velocity change for force measurement
    delv(flag) = delv(flag) + (v(i,2) - vyInitial);
  end
end
