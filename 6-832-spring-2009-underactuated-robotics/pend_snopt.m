function w = pend_snopt()
% pendulum parameters
global m g l I b xdes;
m=1; g = 9.8; l = 1; I = m*l*l; b = 0.1;
xdes = [pi 0]'; % the desired final state

snsummary off;
snseti     ('Major Iteration limit', 100);

%don't worry if SNOPT says "Failed to find optimal solution" when it
%terminates.  So long as the error has dropped below this tolerance,
%the solution is "optimal enough" for our purposes.
snsetr     ('Major optimality tolerance',1e-4);

[alpha,alphalow,alphaupp,alphamul,alphastate,Flow,Fupp,Fmul,Fstate,ObjAdd,ObjRow,    ...
 A,iAfun,jAvar,iGfun,jGvar] = penddata;

[alpha,F,wmul,Fmul,inform]= snsolve( alpha, alphalow, alphaupp, ...
    alphamul, alphastate,    ...
    Flow, Fupp, Fmul, Fstate,       ...
    ObjAdd, ObjRow, A, iAfun, jAvar,...
    iGfun, jGvar, 'pendfun');

snset('Defaults');

% playback the learned policy
x = [0 0]';
dt=.01;
for i=1:251
    draw((i-1)*dt,x);
    x = x + dynamics(x,alpha(i)).*dt;
end

[J,dJdalpha] = pendfun(alpha);
fprintf('\nCost of the found solution: %3.2f\n\n',J)

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [alpha,alphalow,alphaupp,alphamul,alphastate,Flow,Fupp,Fmul,Fstate,ObjAdd,ObjRow, ...
	  A,iAfun,jAvar,iGfun,jGvar] = penddata()

ObjRow = 1;
ObjAdd = 0;

alpha      = ones(251,1);
alphalow   = -inf*ones(251,1);
alphaupp   = inf*ones(251,1);
alphamul   = zeros(251,1);
alphastate = zeros(251,1);

Flow   = -inf;
Fupp   = inf;
Fmul   = 0;
Fstate = 0;

A     = [];
iAfun = [];
jAvar = [];

iGfun = ones(251,1); jGvar = [1:251]';
end

% =============================================================
% This function defines the continuous dynamics of the pendulum
% =============================================================
function xdot = dynamics(x,u)
% pendulum parameters
global m g l I b;

xdot = [x(2,:); (u-m*g*l*sin(x(1,:))-b*x(2,:))./I];
end

% ===============================================================
% This is the draw function
%================================================================
function status = draw(t,x)
persistent hFig base a1 ac1 raarm;

if (isempty(hFig))
    hFig = figure(25);
    set(hFig,'DoubleBuffer', 'on');

    a1 = 0.75;  ac1 = 0.415;
    av = pi*[0:.05:1];
    rb = .03; hb=.07;
    aw = .01;
    base = rb*[1 cos(av) -1 1; -hb/rb sin(av) -hb/rb -hb/rb]';
    arm = [aw*cos(av-pi/2) -a1+aw*cos(av+pi/2)
        aw*sin(av-pi/2) aw*sin(av+pi/2)]';
    raarm = [(arm(:,1).^2+arm(:,2).^2).^.5, atan2(arm(:,2),arm(:,1))];
end

figure(hFig); cla; hold on; view(0,90);
patch(base(:,1), base(:,2),1+0*base(:,1),'b','FaceColor',[.3 .6 .4])
patch(raarm(:,1).*sin(raarm(:,2)+x(1)-pi),...
    -raarm(:,1).*cos(raarm(:,2)+x(1)-pi), ...
    0*raarm(:,1),'r','FaceColor',[.9 .1 0])
plot3(ac1*sin(x(1)), -ac1*cos(x(1)),1, 'ko',...
    'MarkerSize',10,'MarkerFaceColor','b')
plot3(0,0,1.5,'k.')
title(['t = ', num2str(t(1),'%.2f') ' sec']);
set(gca,'XTick',[],'YTick',[])

axis image; axis([-1.0 1.0 -1.0 1.0]);
drawnow;

status = 0;
end