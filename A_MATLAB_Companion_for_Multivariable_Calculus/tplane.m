

%       Function mfile tplane  (tangent plane)
%  Attaches the graph of a piece of the tangent plane to the 
%  graph of f(x,y). The function f can be given in an mfile, or
%  as an inline function. The point p = (x0,y0) is also specified. 
%      The call is  tplane(f,p) when f is given as an inline
%  function, and tplane('f',p) when f is given in an mfile.

function out = tplane(fname,p)

  a = p(1); b = p(2);

  fx = (feval(fname, a+.000001,b) -feval(fname,a,b))*1000000
  fy = (feval(fname, a,b+.000001) - feval(fname,a,b))*1000000

  center = [a b feval(fname,a,b)];
  v = [1,0,fx]; v = v/norm(v);
  w = [0,1,fy]; w = w/norm(w);
  corner1 = center + .4*(v+w);
  corner2 = center + .4*(-v+w);
  corner3 = center + .4*(-v-w);
  corner4 = center + .4*(v-w);
  corners = [corner1; corner2; corner3; corner4];
  xedge = corners(:,1);
  yedge = corners(:,2);
  zedge = corners(:,3);
  fill3(xedge, yedge, zedge, 'c')
  
