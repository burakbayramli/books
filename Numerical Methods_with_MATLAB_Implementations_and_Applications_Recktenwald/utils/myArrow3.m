function myArrow3(x,y,z,u,v,w,headStyle,sf,c,axlim)
% myArrow3  Draw 3D arrows with filled head. Size and color of
%           arrowhead can be specified
%
% Synopsis:  myArrow3(x,y,z,u,v,w)
%            myArrow3(x,y,z,u,v,w,headStyle)
%            myArrow3(x,y,z,u,v,w,headStyle,sf)
%            myArrow3(x,y,z,u,v,w,headStyle,sf,c)
%            myArrow3(x,y,z,u,v,w,headStyle,sf,c,axlim)
%
% Input:     x,y,z = vectors containing coordinates of each vector tail
%            u,v,w = vectors of coordinates of vector tips
%                    If only one vector is drawn, x,y,z, u,v, and w are scalars
%            headStyle = (optional) 1 or 0, indicating plane in which the arrowhead is
%                        drawn.  For headStyle=1 the arrowheads are parallel
%                        to a plane that contains the vector, and is also
%                        normal to the azimuth plane.  For headStyle=0 the
%                        arrowheads lie in the azimuthal plane.
%            sf = (optional) scale factor used to compute length of vector tip.
%                 tip length = sf time maximum length of all vectors input to myArrow3
%                 Default: s = 0.08 times length of the longest vector
%                 If sf<0 then sf is taken to be the absolute length of the tip
%            c = (optional) 1x3 matrix defining the color of an arrowhead.
%                Default:  c = [0 0 0] (black)
%            axlim = (optional) limits for the 3D axis that contains the arrows.
%                    Default:  axlim = [amin amax amin amax amin amax] where
%                    amin is the minimum of all x,y,z values and amax is the
%                    corresponding maximum.  The default has the same effect
%                    as axis('equal')
%
% Output:    3D plot of arrows

debug = 0;
holdState = ishold;  %  Store holdState;  turn hold on/off only if holdState = true
                     %  This prevents interference with hold on/off in calling routine

% --- convert all inputs to column vectors for convenience
x = x(:);  y = y(:);  z = z(:);  u = u(:);  v = v(:);  w = w(:);

if nargin<7,   headStyle = 1;  end 
if nargin<8,   sf = 0.08;      end
if nargin<9,   c = [0 0 0];    end
if nargin<10,  axlim = [];     end

if sf<0
  s = abs(sf);   %  negative sf means abs(sf) is the absolute length of the vector tip
else
  len = sqrt( u.^2 + v.^2 + w.^2 );   %  lengths of each vector
  s = sf*max(len);
end

% --- angles defining the arrowhead
alpha = 12*pi/180;             %  angle of side of arrowhead relative to the shaft
                               %  measured in plane of the arrowhead
theta = atan2(v,u);            %  angle of the shaft relative to x axis (azimuth)
lenxy = sqrt( u.^2 + v.^2 );   %  lengths of projections on (x,y) plane
phi = atan2(w,lenxy);          %  altitude

% --- rows of xx, yy, and zz are tail and tip coordinates of the shaft
xx = [x  x+u];  yy = [y  y+v];  zz = [z  z+w];

% --- hx, hy, and hz are coordinates of triangular patch forming the head
%     Columns are coordinates of the triangle defining the head.  Point 1
%     and point 4 are the same so that polygon is closed.  The hx, hy, and hz
%     depend on the style of the arrowhead.  Currently there are two styles
%     which define the arrowhead in one of two planes.

if headStyle   %  define arrowheads in xy plane and project upward

  if any(tan(phi)) > 100
    fprintf('Arrowheads of some vectors may be distorted\n');
    fprintf('Try headStyle = 0, instead\n');
  end
  [hx,hy,hz] = headInTiltxyPlane(alpha,phi,theta,s,xx,yy,x,y,z,u,v,w,debug);
 
else  %  define arrowheads in azimuthal plane and project over to y=0 and x=0 planes
  [hx,hy,hz] = headInAzPlane(alpha,phi,theta,s,xx,yy,x,y,z,u,v,w,debug);
end


% --- Draw vectors -----------------
if ~holdState,  hold on;  end
for k=1:length(x)
  plot3(xx(k,:),yy(k,:),zz(k,:));
  fill3(hx(k,:),hy(k,:),hz(k,:),c);
end

view(3)  %  make sure perspective on plot shows 3D.  The need for this seems like a bug

% --- Draw axes only if axlim is supplied as input.
if ~isempty(axlim)
  axis(axlim);                           %  Set axis limits (has effect of scaling the plot)
  plot3(axlim(1:2),[0 0],[0 0],'m--');   %  Draw the x,y and z axes
  plot3([0 0],axlim(3:4),[0 0],'m--');
  plot3([0 0],[0 0],axlim(5:6),'m--');
end

% --- draw lines along the x,y and z axes.  Using amin and amax, instead of separate
%     min/max values in each coordinate direction has the effect of scaling the three
%     axes to a uniform aspect ratio
%   xmax = max([xx(:); hx(:)]);       xmin = min([xx(:); hx(:)]);     %  find extent of arrows
%   ymax = max([yy(:); hy(:)]);       ymin = min([yy(:); hy(:)]);
%   zmax = max([zz(:); hz(:)]);       zmin = min([zz(:); hz(:)]);
%   amin = min([xmin, ymin, zmin]);   amax = max([xmax, ymax, zmax]); %  absolute min and max
%   plot3([amin amax],[0 0],[0 0],'m--');     %  xaxis
%   plot3([0 0],[amin amax],[0 0],'m--');     %  yaxis
%   plot3([0 0],[0 0],[amin amax],'m--');     %  zaxis

if ~holdState,  hold off;  end


% =================================================================
function [hx,hy,hz] = headInAzPlane(alpha,phi,theta,s,xx,yy,x,y,z,u,v,w,debug)
% headInAzPlane  Define coordinates of arrowhead in the azimuth plane

if debug,  disp('called headInAzPlane');  end

pma = phi-alpha;  ppa = phi+alpha;             %  temporary angle variables

zt = z+w;      pt = sqrt(u.^2+v.^2);           %  z and p coordinate of vector tip
hz = [zt  zt-s*sin(pma)  zt-s*sin(ppa)  zt];
hp = [pt  pt-s*cos(pma)  pt-s*cos(ppa)  pt];   %  coords of head

hx = zeros(size(hp));  hy = hx;                %  pre-allocate
for k=1:size(hx,1)
  hx(k,:) = cos(theta(k))*hp(k,:);             %  project onto x and y axes
  hy(k,:) = sin(theta(k))*hp(k,:);
end
hx = hx + repmat(x,1,4);
hy = hy + repmat(y,1,4);

if debug               %  ---- draw 2D projection ----
  holdState = ishold;  %  Store holdState;  turn hold on/off only if holdState = true
                       %  This prevents interference with hold on/off in calling routine
  f1 = figure;   m = size(xx,1);
  for k=1:m
    plot(xx(k,:),yy(k,:),'r',hx(k,:),hy(k,:),'b');
    if m>1 & k<m,  pause;  end
    if ~holdState,  hold on;   end
  end
  title('Projection onto x-y plane');
  if ~holdState,  hold off;   end
end


% =================================================================
function [hx,hy,hz] = headInTiltxyPlane(alpha,phi,theta,s,xx,yy,x,y,z,u,v,w,debug)
% headInTiltxyPlane  Define coordinates of arrowhead in a plane intially
%                    parallel to xy plane and then tilted through the
%                    elevation angle

if debug,  disp('called headInTiltxyPlane');  end

tma = theta-alpha;  tpa = theta+alpha;        %  temporary angle variables
xt = u+x;   yt = v+y;                         %  (x,y) coordinates of vector tips
hx = [xt  xt-s*cos(tma)  xt-s*cos(tpa)  xt];  %  coords of points around arrowhead
hy = [yt  yt-s*sin(tma)  yt-s*sin(tpa)  yt];

% --- 3 steps to build coords in azimuthal plane, only vector is projected, not absolute coords
hz = sqrt( (hx-repmat(x,1,4)).^2 + (hy-repmat(y,1,4)).^2);
for k=1:size(hz,1)
  hz(k,:) = hz(k,:)*tan(phi(k));         %  project up to tilted xy plane
end
hz = repmat(z,1,size(hz,2)) + hz;        %  add offset of tail

if debug               %  ---- draw 2D projection ----
  holdState = ishold;  %  Store holdState;  turn hold on/off only if holdState = true
                       %  This prevents interference with hold on/off in calling routine
  f1 = figure;   m = size(xx,1);
  for k=1:m
    plot(xx(k,:),yy(k,:),'r',hx(k,:),hy(k,:),'b');
    if m>1 & k<m, pause;  end
    if ~holdState,  hold on;   end
  end
  title('Projection onto x-y plane');  xlabel('x axis');  ylabel('y axis');
  if ~holdState,  hold off;   end
end
