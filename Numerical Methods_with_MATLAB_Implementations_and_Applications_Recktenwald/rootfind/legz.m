function f = legz(theta,w,h,b)
% legz  Evaluate f(theta) for picnic leg geometry.  Use pass-through
%       parameters in fzero to send w, h, and b values to this function
f = w*sin(theta) - h*cos(theta) - b;
