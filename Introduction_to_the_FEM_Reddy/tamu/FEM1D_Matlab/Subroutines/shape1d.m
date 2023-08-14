function [gj,sf,dsf,gdsf,gddsf]=shape1d(h,ielem,npe,xi)
% c     __________________________________________________________________
% c
% c       called in main to compute shape functions and their derivatives
% c     for hermite cubic and lagrange linear, quadratic and cubic elements
% c
% c        x.........  global (i.e., problem) coordinate
% c        xi .......  local (i.e., element) coordinate
% c        h.........  element length
% c        {sf}......  interpolation (or shape) functions
% c        {dsf}.....  first derivative of sf w.r.t. xi
% c        {ddsf}....  second derivative of sfh w.r.t. xi
% c        {gdsf}....  first derivative of sf w.r.t. x
% c        {gddsf}...  second derivative of sfh w.r.t. x
% c        gj........  jacobian of the transformation
% c     __________________________________________________________________
% c
% implicit real*8 (a-h,o-z)
% common/shp/sf(4),gdsf(4),gddsf(4),gj
% dimension dsf(4),ddsf(4)
sf=zeros(4,1); gdsf=zeros(4,1); gddsf=zeros(4,1); dsf=zeros(4,1); ddsf=zeros(4,1);
if(ielem==0)
    % c     hermite interpolation functions (for the euler-bernoulli theory)
    net=4;
    sf(1)  = 0.25*(2.0-3.0*xi+xi^3);
    sf(2)  =  -h*(1.0-xi)*(1.0-xi*xi)/8.0;
    sf(3)  = 0.25*(2.0+3.0*xi-xi^3);
    sf(4)  =   h*(1.0+xi)*(1.0-xi*xi)/8.0;
    dsf(1) = -0.75*(1.0-xi*xi);
    dsf(2) =  h*(1.0+2.0*xi-3.0*xi*xi)/8.0;
    dsf(3) =  0.75*(1.0-xi*xi);
    dsf(4) =  h*(1.0-2.0*xi-3.0*xi*xi)/8.0;
    ddsf(1)= 1.5*xi;
    ddsf(2)= 0.25*h*(1.0-3.0*xi);
    ddsf(3)= -1.5*xi;
    ddsf(4)=-0.25*(1.0+3.0*xi)*h;
else
    net=npe;
    if(ielem==1)
        % c     lagrange interpolation functions used for linear, quadratic and
        % c     cubic approximation of second-order equations linear interpolation functions
        
        sf(1)  = 0.5*(1.0-xi);
        sf(2)  = 0.5*(1.0+xi);
        dsf(1) = -0.5;
        dsf(2) = 0.5;
        ddsf(1)= 0.0;
        ddsf(2)= 0.0;
    else
        if(ielem==2)
            % c quadratic interpolation functions
            sf(1)  = -0.5*xi*(1.0-xi);
            sf(2)  = 1.0-xi*xi;
            sf(3)  = 0.5*xi*(1.0+xi);
            dsf(1) = -0.5*(1.0-2.0*xi);
            dsf(2) = -2.0*xi;
            dsf(3) = 0.5*(1.0+2.0*xi);
            ddsf(1)= 1.0;
            ddsf(2)= -2.0;
            ddsf(3)= 1.0;
        else
            % c     cubic interpolation functions
            sf(1)  = 0.0625*(1.0-xi)*(9.0*xi*xi-1.);
            sf(2)  = 0.5625*(1.0-xi*xi)*(1.0-3.0*xi);
            sf(3)  = 0.5625*(1.0-xi*xi)*(1.0+3.0*xi);
            sf(4)  = 0.0625*(9.0*xi*xi-1.0)*(1.0+xi);
            dsf(1) = 0.0625*(1.0+18.0*xi-27.0*xi*xi);
            dsf(2) = 0.5625*(-3.0-2.0*xi+9.0*xi*xi);
            dsf(3) = 0.5625*(3.0-2.0*xi-9.0*xi*xi);
            dsf(4) = 0.0625*(18.0*xi+27.0*xi*xi-1.0);
            ddsf(1)= 0.0625*(18.0-54.0*xi);
            ddsf(2)= 0.5625*(-2.0+18.0*xi);
            ddsf(3)= 0.5625*(-2.0-18.0*xi);
            ddsf(4)= 0.0625*(18.0+54.0*xi);
        end
    end
end
%c     compute derivatives of the interpolation functions w.r.t. x
gj =  h*0.5;
for i  = 1:net
    gdsf(i)  = dsf(i)/gj;
    gddsf(i) = ddsf(i)/gj/gj;
end
end
