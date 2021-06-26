
% This function mfile slices the graph of f with a plane parallel to
% xz plane. It assumes that the function f has already been graphed
% over the rectangle [a,b]\times [c,d].  The call is yslice(f,x0,y)
% when f is given as an inline function, and yslice('f',x0,y) when
% f is given in an  mfile.  x0 is the constant x value, and y 
% is the vector of y coordinates which ranges from c to d. 


function out = yslice(f, x0, y)

  z = feval(f, x0, y);
  high = max([z+1 0]);
  low =  min([z-1,0]);

  c = min(y);  d = max(y);

  xedge = [x0 x0 x0 x0 x0];
  yedge = [c d d c c];
  zedge = [low low high high low];

  fill3(xedge, yedge, zedge, 'b')
