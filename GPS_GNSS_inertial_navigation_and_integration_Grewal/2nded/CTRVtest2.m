%
% Test accuracy of MATLAB functions RotVec3CTMat & CTMat2RotVec
% 
% M. S. Grewal, L. R. Weill and A. P. Andrews
% Global Positioning Systems, Inertial Navigation, and Integration
% 2nd Edition, Wiley, 2006
%
clear all;
close all;
k=0;
for latk=-90:90,
    k         = k+1;
    latdeg(k) = latk;
    lat       = latk*pi/180;
    slat      = sin(lat);
    clat      = cos(lat);
    for lonn=1:360,
        londeg(lonn) = lonn;
        lon          = lonn*pi/180;
        slon         = sin(lon);
        clon         = cos(lon);
        rho          = pi*[clat*clon;clat*slon;slat];
        rhoout       = CTMat2RotVec(RotVec2CTMat(rho));
        r(k,lonn)    = sqrt(sum((rhoout-rho).^2));
        if r(k,lonn) > pi
            r(k,lonn)    = sqrt(sum((rhoout+rho).^2));
        end;
    end;
end;
figure;
mesh(londeg,latdeg,r);
xlabel('Longitude [deg]');
ylabel('Latitude [deg]');
zlabel('Radial Error');
title('CTMat2RotVec(RotVec2CTMat( \rho ))Error for | \rho | = \pi');
figure;
[rows,cols] = size(r);
n = rows*cols;
z = reshape(r,n,1);
[no,bins] = hist(z/2/pi,0:.001:1);
clear z;
plot(bins,no/n,'r-');
title('CTMat2RotVec(RotVec2CTMat( \rho )) Error Histogram for | \rho | = \pi');
xlabel('|\rho_{Output}-\rho_{Input}|/2\pi');
ylabel(['Rel. No. of Occurrences in ',num2str(n),' Uniform Samples']);