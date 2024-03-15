%
% Rankine oval: Example of a 2D potential flow.
% Prepared by D. T. Valentine, 2016. Reference:
% Section 5.3.7.
%
clear;clc
% INPUTS and display on command window of formulas 
% applied in this MATLAB code:
% disp(' ')
% disp(' Rankine oval')
% m = 10;     % Source strength.
msource = 0.1:0.1:10;
c = .5;    % Half distance between source and sink on x axis.
U = 1;     % Free stream speed in x direction.
% disp(' U ; m ')
% disp([U m])
% disp(' phi = U*x + (m/4/pi)*log((x+c)^2+y^2)-(m/4/pi)*log((x-c)^2+y^2)')
% disp(' psi = U*y + (m/2/pi)*atan2(y,x+c) - (m/2/pi)*atan2(y,x-c)')  
% disp(' u = U + m/2/pi * ((x+c)/((x+c)^2+y^2)-(x-c)/((x-c)^2+y^2))')
% disp(' v = m/2/pi * (y/((x+c)^2+y^2)-y/((x-c)^2+y^2)')
%
for im = 1:length(msource)
    m = msource(im);
xL = -2;
N = 1000;
xR = 2;
xd = xL:(xR-xL)/N:xR;
for n = 1:length(xd)
% Iterative solution of Equation (3.31) for psi = 0:
    yd(n) = m/2/U;
    for it = 1:1000
        yd(n) = -(1/2/U)*(   m/pi*atan2(yd(n),xd(n)+c) ...
            - m/pi*atan2(yd(n),xd(n)-c) );
    end
end
% Calculation of pressure distribution the Rankine oval:
% Offsets of oval: (xd,yd)
xL(1) = xd(end); yL = -yd(end);
for nn = 2:length(xd)-1
xL(nn) = xd(end-nn); yL(nn) = -yd(end-nn);end
if length(msource) == 1
plot([xd xL],[yd yL],'k',[-1 1],[0 0],'k')
end
% Calculation of (u,v) on the surface of the oval:
u = U + m/2/pi * ((xd+c)./((xd+c).^2+yd.^2)-(xd-c)./((xd-c).^2+yd.^2));
v = m/2/pi * (yd./((xd+c).^2+yd.^2)-yd./((xd-c).^2+yd.^2));
% Claculation of the pressure coefficient on oval:
 Cp = 1-(u.^2+v.^2)/U^2;
 % Plot of pressure distribution:
 if length(msource) == 1
 hold on
 plot(xd,Cp)
 end
 xstg1 = -sqrt(c^2+c*m/pi/U);
 xstg2 =  sqrt(c^2+c*m/pi/U);
 ystg = 0;
 if length(msource) == 1
 plot([xstg1 xstg2],[ystg ystg],'ok')
 end
% Iterative solution to find the thickness of the oval 
% with the formula given in Section 3.3.7 (see solution 
% provided for Problem 3.2 --- MATLAB script P3_2mod.m)
yo = 2;
for it = 1:1000
    yo = (m/U)*( 1/2 - 1/pi*atan2(yo,c) );
end
if length(msource) == 1
plot(0,yo,'or')
end
[Cpmin ixd] = min(Cp);
xmin = xd(ixd);
ymin = yd(ixd);
% Plot the location of the minimum pressure coefficient:
if length(msource) == 1
plot(xmin,ymin,'+r')
end
% Output minimum pressure coefficient to command window:
if length(msource) == 1
Cpmin
end
CPM(im) = Cpmin;
% Output thickness-to-chord ratio:
if length(msource) == 1
thickness_to_chord = yo/abs(xstg1)
end
toverc(im)=yo/abs(xstg1);
end
plot(toverc,CPM)
