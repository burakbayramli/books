%SATCONST   Script for drawing GPS constellation
%           in INERTIAL and ECEF frames

%    Written by Kai Borre
%    July 1, 1997

e = exist('satconst.eps');
if e ~= 0, delete('satconst.eps'); end
e = exist('satconin.eps');
if e ~= 0, delete('satconin.eps'); end

% Offsets of PRN labels, set manually
dx = [       0;  %1
       2000000;
       1000000;
      -2000000;
             0; % 5
      -1000000;
             0;
             0; 
       1000000;
      -5000000; %10
             0;
             0;
             0;
       -200000;
       -200000; % 15
       -100000;
       -200000;
        150000;
       -200000;
             0; % 20
        200000;
        200000;
        200000;
        100000;
        300000; % 25
        200000;
       -200000;
             0;
             0;
             0; % 30
             0];   
    
dy = [       0;
             0;
      -3000000;
       3000000;
             0; % 5
       5000000; 
       1000000;
             0;
      -3000000;
        100000; % 10
             0;
             0;
             0;
       1000000;
       2000000; % 15
       3000000;
       3000000;
      -3000000;
       3000000;
             0; % 20
      -6000000;
      -6000000;
      -6000000;
      11000000;
      -2000000; % 25
      -6000000;
       3000000;
             0;
      10000000;
      -2000000;% 30
      -1000000];
   
dz = [-2500000;
       -200000;
       -300000;
       -200000;
       2500000; % 5
       -300000;
      -2500000;
             0;
       -500000;
       -400000; % 10
             0;
             0;
             0;
       2500000;
      -3000000; % 15
      -2500000;
      -2500000;
      -1500000;
      -3500000; 
             0; % 20
       -300000;
       -300000;
       -300000;
       -200000;
       -250000; % 25
       -300000;
      -3500000;
             0;
      -2000000;
       1200000; % 30
       2500000];

eph = get_eph('edata.dat'); 
f2 = figure;
hold on
axis off; axis equal
view([-20 0])  
for t = 1:36
    isat = find(eph(1,:) == t);
    if isempty(isat) == 0
        for t1 = 1:48 
           p(t1,:) = satposin(eph(21,1)+900*t1, eph(:,isat(1)))';
        end
        switch  eph(1,isat(1))
          case {9,19,25,27}   % A
             colstr='c';
          case {2,5,20,22,30} % B
             colstr='m';
          case {3,6,7,28,31}  % C
             colstr='y';
          case {4,15,17,24}   % D
             colstr='r';
          case {14,16,21,23}  % E
             colstr='g';
          case {1,18,26,29}   % F
             colstr='b';
       end;
       orbit = line('color',colstr,'linestyle','-',...
	 	               'erase','none','xdata',[],'ydata',[],'zdata',[]);
  	    set(orbit,'xdata',p(:,1)','ydata',p(:,2)','zdata',p(:,3)');
       % drawnow
       plot3(p(1,1),p(1,2),p(1,3),[colstr '*'],'markersize',12)
       h = text(p(1,1)+dx(t),p(1,2)+dy(t),p(1,3)+dz(t),...
    	      	   ['\bf' int2str(eph(1,isat(1)))]); 
    end             
end
[x,y,z] = sphere(25);
s1 = surf(6700000*x,6700000*y,6700000*z);
% If you have enough memory to rende a big buffer then 
% uncomment the following three lines
% load topo
% set(s1,'facecolor','texturemap','cdata',topo)
% colormap(topomap1);
hold off
set(gcf,'backingstore','off')
print -deps2 satconin.eps

%break

f3 = figure;
X = zeros(96,25); Y = zeros(96,25); Z = zeros(96,25);
p = zeros(96,3);
for prn = 1:36
   isat = find(eph(1,:) == prn);
   if isempty(isat) == 0
       for t = 1:96
           p(t,:) = satpos(eph(21,1)+900*t, eph(:,isat(1)))';
       end
   comet3(p(:,1),p(:,2),p(:,3),.2)
   X = [X p(:,1)];
   Y = [Y p(:,2)];
   Z = [Z p(:,3)];
   end
end
%title('GPS Satellite Orbits as Realized in ECEF Frame')
set(gca,'Visible','off');
hold on
[x,y,z] = sphere(25);
s2 = surf(6700000*x,6700000*y,6700000*z);
for t = 1:size(X,2)
     plot3(X(:,t),Y(:,t),Z(:,t),'c')
     plot3(X(1,t),Y(1,t),Z(1,t),'w*')
end
% load topo
% set(s2,'facecolor','texturemap','cdata',topo)
% colormap(topomap1);
hold off
set(gcf,'backingstore','off')
print -deps2 satconst.eps

%%%%%%%%%%%%% end satconst.m %%%%%%%%%%%%%%%%%%%%%%%
