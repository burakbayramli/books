%Program 3.6
function H=hermit(x0,y0,dy0,x1,y1,dy1,KC,c,d) %Hermite interpolating polynomial
%Input : [x0,y0],dy0 - coordinates of left end point and derivative at the point
%        [x1,y1],dy1 - coordinates of right end point and derivative at the point
%Output: H=coefficients of cubic Hermite interpolating polynomial 
if nargin<9, d=1;  end % used as d2/d1 if dy1=inf/0
if nargin<8, c=1;  end % used as c2/c1 if dy0=inf/0
if nargin<7, KC=0;  end
%If KC~=0 or either of the derivatives at the end points is inf,
% the parameter(intermediate variable) t is introduced(Eq.(3.35)??).
eps =0.0000001;
if abs(dy0)~=inf &abs(dy1)~=inf
  if KC==0
    A =[x0^3    x0^2   x0   1;
        x1^3    x1^2   x1   1;
        3*x0^2  2*x0   1    0;
        3*x1^2  2*x1   1    0];
    b = [y0  y1 dy0 dy1]'; %Eq.(3.32)
    H =(A\b)'; 
   elseif KC<0 % approximating the derivative on boundary by difference(Eq.(3.34))
     x =[x0  x0+eps         x1-eps        x1];
     y =[y0  y0+dy0*eps  y1-dy1*eps y1];
     H =lagranp(x,y);
  end
  return
end
if KC~=0| abs(dy0)==inf |abs(dy1)==inf
  if  dy0==inf,  dx0=0; dy0=c; 
   elseif  dy0==-inf,  dx0=0; dy0=-c;
   else  dx0=c;   dy0 =dy0*c;
  end
  if  dy1==inf,  dx1=0; dy1=d; 
   elseif  dy1==-inf,  dx1=0; dy1=-d;
   else  dx1=d;   dy1 =dy1*d;
  end 
  % to use the parameter t
  if KC>=0 
    A =[0  0  0  1; 
        1  1  1  1;
        0  0  1  0;
        3  2  1  0]; %Eq.(3.36)&(3.37)
    b1 =[x0  x1  dx0  dx1]';   
    H(1,:) =(A\b1)'; 
    b2 =[y0  y1  dy0  dy1]';   
    H(2,:) =(A\b2)'; 
  else
   %% approximating the derivative on boundary to difference(Eq.(3.38))
    t=[0  eps  1-eps  1];
    x=[x0  x0+dx0*eps   x1-dx1*eps  x1];
    y=[y0  y0+dy0 *eps  y1-dy1*eps  y1];
    H(1,:) =lagranp(t,x);
    H(2,:) =lagranp(t,y);
  end
end