function rimlessWheel(w0)

% RimlessWheel
%   rimlessWheel(w0) starts the wheel with w0 as the initial
%   rotation velocity.
%
%   rimlessWheel with no arguments starts the wheel with a random
%   initial velocity.
%
% Written by Russ Tedrake (russt@mit.edu)


m = 1; l = 1; g = 9.8; alpha = pi/8;
%gamma = 0.03;  % standing is only fixed point
gamma = 0.08;  % standing and rolling fixed points
%gamma = alpha+0.01;  % only rolling fixed point

if (nargin < 1)
  w0 = 5*randn;
end

x = [-sign(w0)*alpha+gamma; w0; 0];  % [\theta, \dot\theta, xfoot] 
  % I'm only keeping xfoot around so that the drawing looks smoother

plant_dt = 1e-3;
display_dt = 0.05;
T = 20;

draw(x,0);
last_display_t = 0;
for t=0:plant_dt:T
  if sign(x(2))*(x(1)-gamma) >= alpha % collision
    x = collision(x);
  end
  if (t - last_display_t > display_dt)
    draw(x,t);
    last_display_t = t;
  end
  x = x + plant_dt*stance_dynamics(x);
end
draw(x,T);


  function xp = collision(xm)

    xp = [-sign(xm(1)-gamma)*alpha + gamma; ...
      xm(2)*cos(2*alpha); ...
      xm(3) + sign(x(1)-gamma)*2*l*sin(alpha)];

  end


  function xdot = stance_dynamics(x)

    xdot = [x(2); g*sin(x(1))/l; 0];

  end


  function draw(x,t)

    figure(25);
    set(gcf,'DoubleBuffer','on');

    cla;
    hip = x(3)*[cos(gamma);-sin(gamma)] + l*[sin(x(1));cos(x(1))];
    hold on;
    title(['t = ', num2str(t)]);
    for t=x(1)+[0:2*alpha:pi]
      line(hip(1)+l*sin(t)*[1;-1], hip(2)+l*cos(t)*[1;-1],'Color',[0 0 0],'LineWidth',2);
    end
    t = 0:0.1:2*pi;
    line(hip(1)+0.15*sin(t),hip(2)+0.15*cos(t),'Color',[0 0 0]);
    fill(hip(1)+0.15*sin(t),hip(2)+0.15*cos(t),[ 0.502 1.000 1.000 ]);
    line(hip(1)+[-4,4],tan(-gamma)*(hip(1)+[-4,4]));
    axis equal;

    drawnow;

  end

end