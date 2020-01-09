function [x,v,f,q,s,c,A] = network_flow(GeneratePlots)
% optimal network flow problem
%    minimize    f1(x1)+f2(x2)+...+fn(xn)
%    subject to  A*x = s
% the cost function has the form
%    fi(xi) = |xi|/(ci-|xi|)
% where ci the capacity of the ith link.
%
% GeneratePlots: 0  don't generate eps files for the figures
%                1  generate black-white eps files of figures
%                2  generate color eps files of the figures
%

if nargin == 0
  GeneratePlots = 0;
end;

if GeneratePlots ~= 0;
  close all;
end;

% The incidence matrix
A = [  1  1  0  0  0  0  0 ;
      -1  0  1  1  0  0  0 ;
       0 -1 -1  0  1  1  0 ;
       0  0  0 -1 -1  0  1 ;
       0  0  0  0  0 -1 -1 ];
[p,n] = size(A);

% Capacity of each link
c = [  1; 1; 1; 1; 1; 1; 1];

% Source and sink flows
s = [ 0.2; 0.6; 0; 0; -0.8 ];


% run subgradient method with different stepsize rules
for step_rule = 1:3;

ITES = 100;
ites = (1:ITES)';
primal_value = zeros(ITES,1);
dual_value = zeros(ITES,1);
resid_norm = zeros(ITES,1);
potential = [];
% initialize potential vector (dual variables)
v = zeros(p,1);

for k=1:ITES;
  potential = [potential v];
  
  % the potential drop across each link
  dv = A'*v;

  % the dual function
  f_conj = (sqrt(abs(c.*dv))-1).^2.*((abs(dv)-1./c)>0);
  q = - sum(f_conj) + v'*s;

  % find the proposed flow
  x = zeros(n,1);
  I = find((abs(dv)-1./c)>0);
  x(I) = sign(dv(I)).*(c(I)-sqrt(c(I)./abs(dv(I))));

  % different stepsize rules  
  switch step_rule
   case 1
    alpha = 0.3;
   case 2
    alpha = 1;
   case 3 
    alpha = 3;
  end;
  
  % update the dual variable
  surplus = s - A*x;    % the subgradient
  v(1:p-1) = v(1:p-1) + alpha*surplus(1:p-1);

  % record data
  primal_value(k) = sum( abs(x)./(c-abs(x)));
  dual_value(k) = q;
  resid_norm(k) = norm(surplus);
end;
  
% value of the primal function 
f = sum( abs(x)./(c-abs(x)));

LineTypes = ['k--'; 'b- '; 'r-.'];
switch step_rule
 case {1,2,3}
 % plot the dual function versus iteration number
   figure(1)
   plot(ites,dual_value,LineTypes(step_rule,:),'LineWidth',1.5);
   hold on;
 % plot the primal constraint residual 
   figure(2)
   plot(ites,resid_norm,LineTypes(step_rule,:),'LineWidth',1.5);
   hold on;
   resid_norm(end)
end;

if step_rule == 2
% plot the dual variables versus iteration number
   figure(3)
   plot(ites,potential(1,:)','b-','LineWidth',1.5); hold on;
   plot(ites,potential(2,:)','r-.','LineWidth',1.5);
   plot(ites,potential(3,:)','k:','LineWidth',1.5);
   plot(ites,potential(4,:)','g--','LineWidth',1.5);
   set(gca,'FontSize',18);
   axis([ 0 ITES 0 5.5]);
   legend('v1','v2','v3','v4',4);
   xlabel('k');
   ylabel('v');
   if GeneratePlots == 1
     print -deps network5_potentials.eps
   elseif GeneratePlots == 2
     print -depsc network5_potentials.eps
   else
   end;
end;

end;

% plot the dual function versus iteration number
figure(1)
set(gca,'FontSize',18);
axis([ 0 ITES 0 3]);
legend('const1 val','const2','const3',4);
xlabel('k');
ylabel('q');
if GeneratePlots == 1
  print -deps network5_dual_const.eps
elseif GeneratePlots == 2
  print -depsc network5_dual_const.eps
else
end;

% plot the primal constraint residual 
figure(2)
set(gca,'FontSize',18);
%axis([ 0 ITES 0 1.4]);
legend('const1 val','const2','const3',1);
xlabel('k');
ylabel('Ax-s');
if GeneratePlots == 1
  print -deps network5_resid_const.eps
elseif GeneratePlots == 2
  print -depsc network5_resid_const.eps
else
end;

% Plot the network flows
%
% node coordinates and circle radius
figure(4)
xNode = [0  0    5     5  9 ];
yNode = [2 -2  2.5  -2.5  0 ];
rNode = 0.5;
wNode = 1.5;
Xmax = max(c);
Xmin = 0.001;
hmax = 0.6;
aL = 0.5;
aW = 0.1;
sL = 1;
sAgl = [ pi; pi; 0; 0; 0 ];

plotnetwork(4,A,x,s,xNode,yNode,rNode,wNode, ...
	    hmax,aL,aW,Xmin,sL,sAgl,0,'c',Xmax)

text(0.1,0,'l12');
text(2.85,2.6,'l13');
text(2.5,0.2,'l32');
text(2.2,-1.9,'l24');
text(5.1,0,'l34');
text(6.8,1.7,'l35');
text(6.8,-1.5,'l45');

axis([-2, 11, -3, 3]);
axis equal; 
if GeneratePlots == 1
  print -deps network5_flow_potential.eps
elseif GeneratePlots == 2
  print -depsc network5_flow_potential.eps
else
end;


% plot topology of the network with reference directions on links.
figure(5);
xx = ones(n,1)*Xmin*2;
ss = zeros(p,1);
plotnetwork(5,A,xx,ss,xNode,yNode,rNode,wNode, ...
	    hmax,aL,aW,Xmin,sL,sAgl,0,'k',Xmax)
text(0.1,0,'l12');
text(2.85,2.6,'l13');
text(2.5,0.2,'l32');
text(2.2,-1.9,'l24');
text(5.1,0,'l34');
text(6.8,1.7,'l35');
text(6.8,-1.5,'l45');

axis([-2, 11, -3, 3]);
axis equal; 
if GeneratePlots == 1
  print -deps network_5nodes.eps 
elseif GeneratePlots == 2
  print -depsc network_5nodes.eps 
else
end;



%----------------------------------------------------------------------
function plotnetwork(figid,A,flow,source,xNode,yNode,rNode,wNode, ...
		     hmax,aL,aW,Xmin,sL,sAgl,PosAdj,Xcolor,Xmax)
% plot network flow as a nice picture!
% should preceed with hold on or off.
%
% Input parameters:
% figid --- integer id of figure to plot on
% A --- incidence matrix of the network
% flow --- flow variables on each link
% source --- souce variables at each node
% xNode --- horizontal positions of the nodes
% yNode --- vertical positions of the nodes
% rNode --- the radius of the circle representing a node
% wNode --- line width when drawing the node circles
% hmax --- maximum width for the flow and source arrows 
% aL --- length of the arrow head
% aW --- extra width of the arrow head
% Xmin --- minimum threshold on flows and sources to be plotted
% sL --- length for drawing source/sink arrows
% sAgl --- angle of the source arrows, starting from the nodes
% PosAdj --- 0 no offset, 1 or -1 for offset from central line
% Xcolor --- color for the flow and source arrows

figure(figid);

[Nnodes,Llinks]= size(A);

% first, draw the link flows
if nargin < 17
  Xmax = max( max(flow), max(abs(source)) );
end;

for i=1:Llinks;
  if abs(flow(i))>Xmin
    Isrc = find( sign(flow(i))*A(:,i)>0 );
    Idst = find( sign(flow(i))*A(:,i)<0 );
  else
    Isrc = find( A(:,i)>0 );
    Idst = find( A(:,i)<0 );
  end;  
  xdelta = xNode(Idst)-xNode(Isrc);
  ydelta = yNode(Idst)-yNode(Isrc);
  RotAgl = atan2( ydelta, xdelta );
  xstart = xNode(Isrc) + rNode*cos(RotAgl);
  ystart = yNode(Isrc) + rNode*sin(RotAgl);
  xend = xNode(Idst) - rNode*cos(RotAgl);
  yend = yNode(Idst) - rNode*sin(RotAgl);
  L = sqrt( xdelta^2 + ydelta^2 ) - 2*rNode;
  if abs(flow(i)) > Xmin;
    h = abs(flow(i))/Xmax*hmax;
    drawarrow(xstart, ystart, RotAgl, L, h, aL, aW, PosAdj, Xcolor);
    hold on;
  else;
    plot([xstart xend],[ystart yend],'k--','LineWidth',1.5);
  end;
end;

% now draw the source and sink flows
for i=1:Nnodes;
  if source(i)<-Xmin
    xstart = xNode(i) + rNode*cos(sAgl(i));
    ystart = yNode(i) + rNode*sin(sAgl(i));
    h = abs(source(i))/Xmax*hmax;
    drawarrow( xstart, ystart, sAgl(i), sL, h, aL, aW, 0, Xcolor );
  elseif source(i)>Xmin
    xstart = xNode(i) + (rNode+sL)*cos(sAgl(i));
    ystart = yNode(i) + (rNode+sL)*sin(sAgl(i));
    h = abs(source(i))/Xmax*hmax;
    drawarrow( xstart, ystart, pi+sAgl(i), sL, h, aL, aW, 0, Xcolor );
  else
  end;
end;

% the circle to draw around each node
angle = linspace(0,2*pi,100);
xbd = rNode*cos(angle);
ybd = rNode*sin(angle);

% last, draw the nodes 
for i=1:Nnodes;
  plot( xNode(i)+xbd, yNode(i)+ybd, 'k', 'LineWidth', wNode );
end;
text(xNode,yNode, num2str((1:Nnodes)') );
axis equal; 
set(gca,'Visible','off');


%----------------------------------------------------------------------
function []=drawarrow( x0, y0, RotAngle, L, h, aL, aW, PosAdj, color )    
xp = [     0   L-aL      L-aL L     L-aL  L-aL     0      0];
yp = [-0.5*h -0.5*h -0.5*h-aW 0 0.5*h+aW 0.5*h 0.5*h -0.5*h]; 

if PosAdj == 1
  yp = yp + 0.5*h;
elseif PosAdj == -1
  yp = yp - 0.5*h;
else
  yp = yp;
end;

RotMat = [cos(RotAngle) -sin(RotAngle); sin(RotAngle) cos(RotAngle)];

DrawCoordinates = RotMat*[ xp; yp ];
xd = x0 + DrawCoordinates(1,:);
yd = y0 + DrawCoordinates(2,:);

patch( xd, yd, color );

return;
