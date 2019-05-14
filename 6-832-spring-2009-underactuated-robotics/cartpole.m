function cartpole

% Parameters:
mc = 10; mp = 1; l = 0.5; g = 9.8;
%mc = 1; mp = 1; l = 1; g = 1;
T = 15;
plant_dt = 1e-3;
display_dt = 0.1;


% Initial Conditions:  
randn('state',sum(100*clock))
%   x = [x,\theta,\dot{x},\dot\theta]^T 
%   (set this to [0;0;0;0] to test true swing-up)
x = 0.1*randn(4,1);  
%x = zeros(4,1);


% Euler Integration Loop:
last_display_t = -inf;
for t=0:plant_dt:T
  u = control(x,t);

  if (t>last_display_t + display_dt)
    draw(x,t);
    last_display_t = t;

    % Energy calc (good way to verify eqs. of motion)
%    T = 0.5*(mc+mp)*x(3)^2 + mp*x(3)*x(4)*l*cos(x(2)) + 0.5*mp*l^2*x(4)^2;
%    U = -mp*g*l*cos(x(2));
%    E = T+U
  end
  
  xdot = dynamics(x,u);
  x = x + plant_dt*xdot;
end
draw(x,t);


  function u = control(x,t)
    %  TO DO: Fill in your control here
    u = 0;
  end
  
  function xdot = dynamics(x,u)
    s = sin(x(2)); c = cos(x(2));

%    H = [mc+mp, mp*l*c; mp*l*c, mp*l^2];
%    C = [0 -mp*x(4)*l*s; 0 0];
%    G = [0; mp*g*l*s];
%    B = [1; 0];
%    xdot = [x(3:4); inv(H)*[B*u - C*x(3:4) - G]];

    xddot = [u + mp*s*(l*x(4)^2 + g*c)]/[mc+mp*s^2];
    tddot = [-u*c - mp*l*x(4)^2*c*s - (mc+mp)*g*s]/[l*(mc+mp*s^2)];
    xdot = [x(3:4); xddot; tddot];
  end


  function draw(x,t)
    persistent hFig base a1 raarm wb lwheel rwheel;
    if (isempty(hFig))
      hFig = figure(25);
      set(hFig,'DoubleBuffer', 'on');

      a1 = l+0.25;
      av = pi*[0:.05:1];
      theta = pi*[0:0.05:2];
      wb = .3; hb=.15;
      aw = .01;
      wheelr = 0.05;
      lwheel = [-wb/2 + wheelr*cos(theta); -hb-wheelr + wheelr*sin(theta)]';
      base = [wb*[1 -1 -1 1]; hb*[1 1 -1 -1]]';
      arm = [aw*cos(av-pi/2) -a1+aw*cos(av+pi/2)
        aw*sin(av-pi/2) aw*sin(av+pi/2)]';
      raarm = [(arm(:,1).^2+arm(:,2).^2).^.5, atan2(arm(:,2),arm(:,1))];
    end

    figure(hFig); cla; hold on; view(0,90);
    patch(x(1)+base(:,1), base(:,2),0*base(:,1),'b','FaceColor',[.3 .6 .4])
    patch(x(1)+lwheel(:,1), lwheel(:,2), 0*lwheel(:,1),'k');
    patch(x(1)+wb+lwheel(:,1), lwheel(:,2), 0*lwheel(:,1),'k');
    patch(x(1)+raarm(:,1).*sin(raarm(:,2)+x(2)-pi),-raarm(:,1).*cos(raarm(:,2)+x(2)-pi), 1+0*raarm(:,1),'r','FaceColor',[.9 .1 0])
    plot3(x(1)+l*sin(x(2)), -l*cos(x(2)),1, 'ko',...
      'MarkerSize',10,'MarkerFaceColor','b')
    plot3(x(1),0,1.5,'k.')
    title(['t = ', num2str(t,'%.2f') ' sec']);
    set(gca,'XTick',[],'YTick',[])

    axis image; axis([-2.5 2.5 -1.5*l 1.5*l]);
    drawnow;
  end


end


