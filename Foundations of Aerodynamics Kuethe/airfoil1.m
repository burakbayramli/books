function [cl,cmac,xp,yp,cp,V,gamma]=airfoil(XB,YB,ALPHA)
% MATLAB code based on FORTRAN code in Kuethe & Chow pages 161 - 163
%
% aifoil.m computes the airfoil lift coefficient, pitching moment coefficient about the
% aerodynamic center, as well as the pressure coefficient distribution, the velocity
% distribution, and vortex sheet strength distribution around the airfoil. The airfoil
% contour is approximated by vortex panels of linearly varying strength.
%
% Input variables:
% XB row vector containing the x-coordinates of the airfoil section
% YB row vector containing the y-coordinates of the airfoil section
% ALPHA angle of attack in degrees
%
% Vectors XB and YB must have the same size. The number of panels equals the size of vectors
% XB and YB minus one. The first and last element must be the location of the trailing edge
% of the airfoil.
%
% Output variables:
% cl section lift coefficient
% cmac pitching moment coefficient about the aerodynamic center
% xp x location of the center point of the panels
% yp y location of the center point of the panels
% cp column vector containing the pressure coefficient at each panel. The values
% of cp are evaluated at the center point of the panel given by (xp, yp)
% V column vector containing the velocity divided by the free stream
% velocity at each panel. The values of the velocity are evaluated at the center
% point of the panel given by (xp, yp)
% gamma column vector containing the normalized circulation of the panels. Note that
% the values of gamma are the strength of the vortex sheet at points of the
% of the airfoil given by the arrays (XB, YB)
%
% Usage:
% 1. - Typical call to obtain all the performance parameters are:
% >XB=[1 0.933 0.75 0.5 0.25 0.067 0 0.067 0.25 0.5 0.75 0.933 1];
% >YB=[0 -0.005 -0.017 -0.033 -0.042 -0.033 0 0.045 0.076 0.072 0.044 0.013 0];
% >ALPHA=8;
% >[cl,cmac,xp,yp,cp,V,gamma]=airfoil(XB,YB,ALPHA);
% >cl,cmac
% University of Notre Dame, AME 350: Aerodynamics 4
% cl =
% 1.1792
% cmac =
% -0.0792
% >cp
% ans =
% Columns 1 through 7
% 0.2630 0.1969 0.2097 0.2667 0.4707 0.9929 -1.8101
% Columns 8 through 12
% -1.5088 -0.9334 -0.5099 -0.1688 0.1674
%
% 2. If you are interested only on cl and cmac the simpler call shown below
% could be used.
% >XB=[1 0.933 0.75 0.5 0.25 0.067 0 0.067 0.25 0.5 0.75 0.933 1];
% >YB=[0 -0.005 -0.017 -0.033 -0.042 -0.033 0 0.045 0.076 0.072 0.044 0.013 0];
% >ALPHA=8;
% >[cl,cmac]=airfoil(XB,YB,ALPHA);
% >cl,cmac
% cl =
% 1.1792
% cmac =
% -0.0792
% NOTE: In these examples > is the MATLAB command prompt
if size(XB,1)~=1 || size(YB,1) ~=1
error ('airfoil specification error-XB & YB must be row vectors -transpose your arrays!')
end
[XMIN,ILE]=min(XB); MP1=size(XB,2); M=MP1-1; ALPHA1=ALPHA*pi/180;
if (YB(1)~=0||YB(MP1)~=0)
error ('airfoil specification error, Y(0) and Y(end) must be zero')
end
if (XB(1)~=XB(MP1)||YB(ILE)~=0)
error ('airfoil specification error')
end
XB=XB'; YB=YB'; c=XB(1)-XMIN; XAC=XMIN+c/4; RHS=zeros(MP1,1);
% University of Notre Dame, AME 350: Aerodynamics 5
gamma=zeros(MP1,1); AN=zeros(MP1,MP1); AT=zeros(M,MP1);
THETA=zeros(M,1); X=zeros(M,1); Y=zeros(M,1); S=zeros(M,1);
SX=zeros(M,1); SY=zeros(M,1); SINE=zeros(M,1); COSINE=zeros(M,1);
X=(XB([1:M])+XB([2:MP1]))/2; Y=(YB([1:M])+YB([2:MP1]))/2;
SX=XB([2:MP1])-XB([1:M]); SY=YB([2:MP1])-YB([1:M]);
S=sqrt(SX.^2+SY.^2); THETA=atan2(SY,SX); SINE=sin(THETA);
COSINE=cos(THETA);
for I=1:M
for J=1:M
if I==J
CN1(I,J)=-1.0;
CN2(I,J)=1.0;
CT1(I,J)=pi/2;
CT2(I,J)=pi/2;
else
XTEMP=X(I)-XB(J);
YTEMP=Y(I)-YB(J);
A=-XTEMP*COSINE(J)-YTEMP*SINE(J);
B=XTEMP^2+YTEMP^2;
TTEMP=THETA(I)-THETA(J);
C=sin(TTEMP);
D=cos(TTEMP);
E=XTEMP*SINE(J)-YTEMP*COSINE(J);
F=log(1+S(J)*(S(J)+2*A)/B);
G=atan2(E*S(J),B+A*S(J));
TTEMP=TTEMP-THETA(J);
P=XTEMP*sin(TTEMP)+YTEMP*cos(TTEMP);
Q=XTEMP*cos(TTEMP)-YTEMP*sin(TTEMP);
CN2(I,J)=D+Q*F/(2*S(J))-(A*C+D*E)*G/S(J);
CN1(I,J)=D*F/2+C*G-CN2(I,J);
CT2(I,J)=C+P*F/(2*S(J))+(A*D-C*E)*G/S(J);
CT1(I,J)=C*F/2-D*G-CT2(I,J);
end
end
end
% Compute influence coefficients
AN=[CN1(:,1),CN1(:,[2:M])+CN2(:,[1:M-1]),CN2(:,M);...
1,zeros(1,M-1),1];
% University of Notre Dame, AME 350: Aerodynamics 6
AT=[CT1(:,1),CT1(:,[2:M])+CT2(:,[1:M-1]),CT2(:,M)];
RHS=[sin(THETA-ALPHA1);0];
% Compute circulation
gamma=AN\RHS;
% Compute velocity
V=cos(THETA-ALPHA1)+AT*gamma;
% Compute pressure coefficient
cp=1-V.^2;
% Compute total circulation
G=S'*(gamma([1:M])+gamma([2:MP1]))/2;
% Compute lift coefficient
cl=4*pi*G/c;
%Compute pitching moment about aerodynamic center
%ISurf=sign([1:M]-ILE);ISurf(ILE)=1;
cmac=S'*(cp.*((X-XAC).*COSINE+Y.*SINE)/c^2); xp=X; yp=Y;
