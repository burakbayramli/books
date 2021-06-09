function myArrow(x,y,u,v,sc,c)
% myArrow  Draw 2D arrows with filled tip(s).
%          Head size and color can be specified
%
% Synopsis:  myArrow(x,y,u,v);
%            myArrow(x,y,u,v,s);
%            myArrow(x,y,u,v,s,c);
%
% Input:     x,y = coordinates of tail in (x,y) plane
%            u,v = length of the vector
%            s = (optional) scalar or vector of scale factors
%                for the tip of the arrow.  s = [ sl, sa ]
%                where sl = scale factor for the length of
%                the vector tip, and sa = scale factor for the
%                angle of the vector tip.  sa is needed only
%                if the scale of the x and y axes are significantly
%                different.  If s is provided as a scalar, the
%                scalar value is assigned sl and sa is taken to be 1.
%                Default:  sl = 0.08*max( max(abs(u)), max(abs(v)) )
%                i.e., 8 percent of the max vector length
%                Default:  sa = 1
%            c = (optional) color of the arrow head
%                Default:  c = [0 0 0]  (black)

x = x(:);  y = y(:);  u = u(:);  v = v(:);

if nargin<5,  sc = [ 0.08*max([ max(abs(u)), max(abs(v)) ]) , 1 ] ;   end
if nargin<6,  c = [0 0 0];         end    %  black
if length(sc)==1,   sc = [sc, 1];  end

sl = sc(1);  sa = sc(2);
alpha = sa*12*pi/180;    %  angle of arrowhead relative to the shaft
theta = atan2(v,u);      %  angle of the shaft relative to x axis
tma = theta-alpha;
tpa = theta+alpha;

% --- xx and yy are coordinates of the shaft
xx = [x  x+u];
yy = [y  y+v];

% --- hx and hy are coordinates of triangular patch forming the head
hx = [x+u  x+u-sl*cos(tma)  x+u-sl*cos(tpa)  x+u];
hy = [y+v  y+v-sl*sin(tma)  y+v-sl*sin(tpa)  y+v];

hold on
for k=1:length(x)
  plot(xx(k,:),yy(k,:));
  fill(hx(k,:),hy(k,:),c);
end
hold off
