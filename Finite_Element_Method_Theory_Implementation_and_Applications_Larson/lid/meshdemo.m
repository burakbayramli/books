function [ p, t ] = distmesh_demo( cases, do_plot )
% DISTMESH_DEMO Run distmesh example demos.
%
%   [ P, T ] = DISTMESH_DEMO( CASES, DO_PLOT ) Run distmesh example
%   demos. CASES may be used to specify the distmesh examples to run
%   (default 1:10). DO_PLOT specifies whether to plot or not (default
%   true).
%
%   Example 1: (Uniform mesh on unit circle)
%      fd = @(p) sqrt(sum(p.^2,2)) - 1;
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.2, [-1,-1;1,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 2: (Uniform mesh on ellipse)
%      fd = @(p) p(:,1).^2/2^2 + p(:,2).^2/1^2 - 1;
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.2, [-2,-1;2,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 3: (Uniform mesh on unit square)
%      fd = @(p) -min(min(min(1+p(:,2),1-p(:,2)),1+p(:,1)),1-p(:,1));
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.2, [-1,-1;1,1], [-1,-1;-1,1;1,-1;1,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 4: (Uniform mesh on complex polygon)
%      pv = [-0.4 -0.5;0.4 -0.2;0.4 -0.7;1.5 -0.4;0.9 0.1;
%            1.6 0.8;0.5 0.5;0.2 1;0.1 0.4;-0.7 0.7;-0.4 -0.5];
%      fd = { 'l_dpolygon', [], pv };
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.1, [-1,-1; 2,1], pv );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 5: (Rectangle with circular hole, refined at circle boundary)
%      drectangle = @(p,x1,x2,y1,y2) -min(min(min(-y1+p(:,2),y2-p(:,2)),-x1+p(:,1)),x2-p(:,1));
%      fd = @(p) max( drectangle(p,-1,1,-1,1), -(sqrt(sum(p.^2,2))-0.5) );
%      fh = @(p) 0.05 + 0.3*(sqrt(sum(p.^2,2))-0.5);
%      [p,t] = distmesh( fd, fh, 0.05, [-1,-1;1,1], [-1,-1;-1,1;1,-1;1,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 6: (Square, with size function point and line sources)
%      dcircle = @(p,xc,yc,r) sqrt((p(:,1)-xc).^2+(p(:,2)-yc).^2)-r;
%      fd = @(p) -min(min(min(p(:,2),1-p(:,2)),p(:,1)),1-p(:,1));
%      dpolygon = @(p,v) feval('l_dpolygon',p,v);
%      fh = @(p) min(min(0.01+0.3*abs(dcircle(p,0,0,0)), ...
%                        0.025+0.3*abs(dpolygon(p,[0.3,0.7;0.7,0.5;0.3,0.7]))),0.15);
%      [p,t] = distmesh( fd, fh, 0.01, [0,0;1,1], [0,0;1,0;0,1;1,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 7: (NACA0012 airfoil)
%      hlead = 0.01; htrail = 0.04; hmax = 2; circx = 2; circr = 4;
%      a = 0.12/0.2*[0.2969,-0.126,-0.3516,0.2843,-0.1036];
%      fd = @(p) max( dcircle(p,circx,0,circr), ...
%                     -((abs(p(:,2))-polyval([a(5:-1:2),0],p(:,1))).^2-a(1)^2*p(:,1)) );
%      fh = @(p) min(min(hlead+0.3*dcircle(p,0,0,0),htrail+0.3*dcircle(p,1,0,0)),hmax);
%
%      fixx = 1 - htrail*cumsum(1.3.^(0:4)');
%      fixy = a(1)*sqrt(fixx) + polyval([a(5:-1:2),0],fixx);
%      pfix = [[circx+[-1,1,0,0]*circr; 0,0,circr*[-1,1]]'; 0,0; 1,0; fixx,fixy; fixx,-fixy];
%      bbox = [circx-circr,-circr; circx+circr,circr];
%      h0   = min([hlead,htrail,hmax]);
%      [p,t] = distmesh( fd, fh, h0, bbox, pfix );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 8: (Uniform mesh on unit sphere)
%      fd = @(p) sqrt(sum(p.^2,2)) - 1;
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.2, [-1,-1,-1;1,1,1] );
%      f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
%      patch( 'vertices', p, 'faces', f, 'facecolor', [.9, .9, .9] )
%
%   Example 9: (Uniform mesh on unit cube)
%      fd = @(p) -min(min(min(min(min(p(:,3),1-p(:,3) ),p(:,2)),1-p(:,2)),p(:,1)),1-p(:,1));
%      fh = @(p) ones(size(p,1),1);
%      pfix = [-1,-1,-1;-1,1,-1;1,-1,-1;1,1,-1; -1,-1,1;-1,1,1;1,-1,1;1,1,1];
%      [p,t] = distmesh( fd, fh, 0.2, [-1,-1,-1;1,1,1], pfix );
%      f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
%      patch( 'vertices', p, 'faces', f, 'facecolor', [.9, .9, .9] ), view(3)
%
%   Example 10: (Uniform mesh on cylinder)
%      fd = @(p) -min(min(p(:,3),4-p(:,3)),1-sqrt(sum(p(:,1:2).^2,2)));
%      fh = @(p) ones(size(p,1),1);
%      pfix = [-1,-1,-1;-1,1,-1;1,-1,-1;1,1,-1; -1,-1,1;-1,1,1;1,-1,1;1,1,1];
%      [p,t] = distmesh( fd, fh, 0.5, [-1,-1,0;1,1,4], [] );
%      f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
%      patch( 'vertices', p, 'faces', f, 'facecolor', [.9, .9, .9] ), view(3)
%
%   See also DISTMESH.

%   Copyright (C) 2004-2012 Per-Olof Persson, 2018 Precise Simulation Limited.
%   See COPYRIGHT.TXT for details.

close all
clc

if( ~nargin || isempty(cases) )
  cases = 1:10;
end
if( nargin<2 )
  do_plot = true;
end

for i=cases
  eval( ['[p,t] = test_case',num2str(i),'(',num2str(do_plot),');'] );
  if( do_plot && i~=cases(end) )
    pause
  end
  fprintf('\n\n')
end

if( ~nargout )
  clear p t
end


function [p,t] = test_case1( do_plot )
s = 'Example 1: (Uniform mesh on unit circle)';
disp(s)
fd = @(p) sqrt(sum(p.^2,2)) - 1;
fh = @(p) ones(size(p,1),1);
[p,t] = distmesh( fd, fh, 0.2, [-1,-1;1,1] );

if( do_plot )
  clf
  patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
  title(s)
  axis tight
  axis equal
end

function [p,t] = test_case2( do_plot )
s = 'Example 2: (Uniform mesh on ellipse)';
disp(s)
fd = @(p) p(:,1).^2/2^2 + p(:,2).^2/1^2 - 1;
fh = @(p) ones(size(p,1),1);
[p,t] = distmesh( fd, fh, 0.2, [-2,-1;2,1] );

if( do_plot )
  clf
  patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
  title(s)
  axis tight
  axis equal
end

function [p,t] = test_case3( do_plot )
s = 'Example 3: (Uniform mesh on unit square)';
disp(s)
fd = @(p) -min(min(min(p(:,2),1-p(:,2)),p(:,1)),1-p(:,1));
fh = @(p) ones(size(p,1),1);
[p,t] = distmesh( fd, fh, 0.2, [-1,-1;1,1], [-1,-1;-1,1;1,-1;1,1] );

if( do_plot )
  clf
  patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
  title(s)
  axis tight
  axis equal
end

function [p,t] = test_case4( do_plot )
s = 'Example 4: (Uniform mesh on complex polygon)';
disp(s)
pv = [-0.4 -0.5;0.4 -0.2;0.4 -0.7;1.5 -0.4;0.9 0.1;
      1.6 0.8;0.5 0.5;0.2 1;0.1 0.4;-0.7 0.7;-0.4 -0.5];
fd = { 'l_dpolygon', [], pv };
fh = @(p) ones(size(p,1),1);
[p,t] = distmesh( fd, fh, 0.1, [-1,-1; 2,1], pv );

if( do_plot )
  clf
  patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
  title(s)
  axis tight
  axis equal
end

function [p,t] = test_case5( do_plot )
s = 'Example 5: (Rectangle with circular hole, refined at circle boundary)';
disp(s)
drectangle = @(p,x1,x2,y1,y2) -min(min(min(-y1+p(:,2),y2-p(:,2)),-x1+p(:,1)),x2-p(:,1));
fd = @(p) max( drectangle(p,-1,1,-1,1), -(sqrt(sum(p.^2,2))-0.5) );
fh = @(p) 0.05 + 0.3*(sqrt(sum(p.^2,2))-0.5);
[p,t] = distmesh( fd, fh, 0.05, [-1,-1;1,1], [-1,-1;-1,1;1,-1;1,1] );

if( do_plot )
  clf
  patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
  title(s)
  axis tight
  axis equal
end

function [p,t] = test_case6( do_plot )
s = 'Example 6: (Square, with size function point and line sources)';
disp(s)
dcircle = @(p,xc,yc,r) sqrt((p(:,1)-xc).^2+(p(:,2)-yc).^2)-r;
fd = @(p) -min(min(min(p(:,2),1-p(:,2)),p(:,1)),1-p(:,1));
fh = @(p) min(min(0.01+0.3*abs(dcircle(p,0,0,0)), ...
                  0.025+0.3*abs(l_dpolygon(p,[0.3,0.7;0.7,0.5;0.3,0.7]))),0.15);
[p,t] = distmesh( fd, fh, 0.01, [0,0;1,1], [0,0;1,0;0,1;1,1] );

if( do_plot )
  clf
  patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
  title(s)
  axis tight
  axis equal
end

function [p,t] = test_case7( do_plot )
s = 'Example 7: (NACA0012 airfoil)';
disp(s)
hlead = 0.01; htrail = 0.04; hmax = 2; circx = 2; circr = 4;
a = 0.12/0.2*[0.2969,-0.126,-0.3516,0.2843,-0.1036];
dcircle = @(p,xc,yc,r) sqrt((p(:,1)-xc).^2+(p(:,2)-yc).^2)-r;
fd = @(p) max( dcircle(p,circx,0,circr), ...
               -((abs(p(:,2))-polyval([a(5:-1:2),0],p(:,1))).^2-a(1)^2*p(:,1)) );
fh = @(p) min(min(hlead+0.3*dcircle(p,0,0,0),htrail+0.3*dcircle(p,1,0,0)),hmax);

fixx = 1 - htrail*cumsum(1.3.^(0:4)');
fixy = a(1)*sqrt(fixx) + polyval([a(5:-1:2),0],fixx);
pfix = [[circx+[-1,1,0,0]*circr; 0,0,circr*[-1,1]]'; 0,0; 1,0; fixx,fixy; fixx,-fixy];
bbox = [circx-circr,-circr; circx+circr,circr];
h0   = min([hlead,htrail,hmax]);

[p,t] = distmesh( fd, fh, h0, bbox, pfix );

if( do_plot )
  clf
  patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
  title(s)
  axis tight
  axis equal
end

function [p,t] = test_case8( do_plot )
s = 'Example 8: (Uniform mesh on unit sphere)';
disp(s)
fd = @(p) sqrt(sum(p.^2,2)) - 1;
fh = @(p) ones(size(p,1),1);
[p,t] = distmesh( fd, fh, 0.2, [-1,-1,-1;1,1,1] );

if( do_plot )
  clf
  f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
  patch( 'vertices', p, 'faces', f, 'facecolor', [.9, .9, .9] )
  title(s)
  view(3)
  rotate3d('on')
  axis tight
  axis equal
end

function [p,t] = test_case9( do_plot )
s = 'Example 9: (Uniform mesh on unit cube)';
disp(s)
fd = @(p) -min(min(min(min(min(p(:,3),1-p(:,3) ),p(:,2)),1-p(:,2)),p(:,1)),1-p(:,1));
fh = @(p) ones(size(p,1),1);
pfix = [-1,-1,-1;-1,1,-1;1,-1,-1;1,1,-1; -1,-1,1;-1,1,1;1,-1,1;1,1,1];
[p,t] = distmesh( fd, fh, 0.2, [-1,-1,-1;1,1,1], pfix );

if( do_plot )
  clf
  f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
  patch( 'vertices', p, 'faces', f, 'facecolor', [.9 .9 .9] )
  title(s)
  view(3)
  rotate3d('on')
  axis tight
  axis equal
end

function [p,t] = test_case10( do_plot )
s = 'Example 10: (Uniform mesh on cylinder)';
disp(s)
fd = @(p) -min(min(p(:,3),4-p(:,3)),1-sqrt(sum(p(:,1:2).^2,2)));
fh = @(p) ones(size(p,1),1);
pfix = [-1,-1,-1;-1,1,-1;1,-1,-1;1,1,-1; -1,-1,1;-1,1,1;1,-1,1;1,1,1];
[p,t] = distmesh( fd, fh, 0.5, [-1,-1,0;1,1,4], [] );

if( do_plot )
  clf
  f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
  patch( 'vertices', p, 'faces', f, 'facecolor', [.9 .9 .9] )
  title(s)
  view(3)
  rotate3d('on')
  axis tight
  axis equal
end


%------------------------------------------------------------------------------%
function [ dist ] = l_dpolygon( p, v )

n_p = size(p,1);
n_s = size(v,1)-1;

dist = zeros(n_s,n_p);
for i_s=1:n_s
  v_i = v([i_s,i_s+1],:);
  n_p = size(p,1);
  w  = v_i(2,:)-v_i(1,:);
  ix1 = ones(n_p,1);
  vp = v_i(ix1,:)-p;
  w1 = w(ix1,:);
  s = dot(w1,vp,2);
  u = -s/(w*w.');
  u(u<0) = 0;
  u(u>1) = 1;
  h = w1.*[u,u]+vp;
  dist(i_s,:) = sqrt(dot(h,h,2));
end
dist = (-1).^(inpolygon(p(:,1),p(:,2),v(:,1),v(:,2))).*min(dist).';
