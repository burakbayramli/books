%
% compute 2D eigensystem given sxx syy 2sxy 
% first major stress fms will be the most extensive
% second major stress the least extensive, most compressive
%
% azi is the angle to the first major stress axis (most extensive)
% in degrees clockwise from north (direction 2, y-axis)

function [ s1 s2 azi ] = eig2d(sigma)

%
% 90/pi from 2\theta = atan((2\sigmax_{xy})(\sigma_{xx}-\sigma_{yy})
%
fac = 28.64788975654116;

s11=sigma(:,1);
s12=sigma(:,3)/2;
s22=sigma(:,2);

x1 = (s11 + s22)/2.0;
x2 = (s11 - s22)/2.0;
r = x2 .* x2 + s12 .* s12;

if(r > 0.0)
    r = sqrt(r);
    s1 = x1 + r;
    s2 = x1 - r;
else
    s1 = x1; 
    s2 = x1;
end
if(x2 ~= 0.0)
    deg = fac * atan2(s12,x2);
elseif(s12 <= 0.0)
        deg= -45.0;
else
    deg=  45.0;
end

azi = 90.0 - deg;
    
end
