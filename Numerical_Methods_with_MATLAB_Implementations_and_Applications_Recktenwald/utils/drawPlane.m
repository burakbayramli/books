function drawPlane(a,bounds,axlim,addAxes)
% drawPlane  Draws a plane in 3D
%
% Synopsis:  drawPlane(a)
%            drawPlane(a,bounds)
%            drawPlane(a,bounds,axlim)
%            drawPlane(a,bounds,addAxes)
%
% Input:  a = vector of coefficients defining the plane
%               z = a(1)*x + a(2)*y + a(3)
%         bounds = (optional) [ xmin  xmax  ymin  ymax] vector defining the
%                  edges of the plane.  Default: bounds = [-1 -1 1 1]
%         axlim  = (optional) [ xmin  xmax  ymin  ymax  zmin  zmax] vector
%                  defining the limits of the (x,y,z) axes.
%                  Default: axlim = [bounds  floor(min(zb))  ceil(max(zb))]
%         addAxes = (optional, flag) determines whether unlabeled axes through
%                   origin are added to the plot.  This is useful if the plane
%                   is centered near the origin.  MATLAB draws axes at the edges
%                   of the plot region.  Default:  addAxes = 0;  no axes added
%
% Output: 3D plot with plane displayed in light grayscale

if nargin<2,  bounds = [-1 -1 1 1];  end  % [ xmin  xmax  ymin   ymax]

% --- Evaluate z coordinate at four points in bounds
xb = [bounds(1);  bounds(1);  bounds(2);  bounds(2)];
yb = [bounds(3);  bounds(4);  bounds(4);  bounds(3)];
zb = a(1)*xb + a(2)*yb + a(3);

if nargin<3 | isempty(axlim)
  axlim = [bounds  floor(min(zb))  ceil(max(zb))];
end

% --- Draw light gray plane: best for black and white laser printers
colormap('gray');
fill3(xb,yb,zb,[0.75 0.75 0.75]);
grid on
axis(axlim)
view(3)

if nargin<4,  addAxes = 0;  end    %  Don't draw axes by default
if addAxes==0,  return;  end
% --- Add x,y,z axes through origin:  useful if plane is centered near origin
hold on
plot3([bounds(1) bounds(3)],[0 0],[0 0],'k');
plot3([0 0],[bounds(2) bounds(4)],[0 0],'k');
plot3([0 0],[0 0],[floor(min(zb)) ceil(max(zb))],'k');
hold off

