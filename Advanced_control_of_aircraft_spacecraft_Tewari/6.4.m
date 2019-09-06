% (c) 2010 Ashish Tewari
global mu; mu=398600.4e9;
global R0; R0=6378.14e3;
global omega; omega = 2*pi/(23*3600+56*60+4.0905);%rad/sec
global hf; hf=120e3;
global delf; delf=26*pi/180;
global lamf; lamf=-82*pi/180;
global S; S =4; %reference base area, m2
global c; c=0.5; % nose-radius, m
global tb1; tb1=100; %first-stage burn-time (s)
global tb2; tb2=175; %second-stage burn-time (s)
global fT1;fT1=824532.879800791/2; %first-stage thrust (N)
global fT2;fT2=161304.620971835*(1.6/1.75); %second-stage thrust (N)
global m01; m01=30856.5129807023; %first-stage initial mass (kg)
global m02; m02=8262.36174702614; %second-stage initial mass (kg)
global mp1; mp1=21012.5606473188; %first-stage propellant mass (kg)
global mp2; mp2=7516.74365967484; %second-stage propellant mass (kg)
global mL; mL=350; %payload mass (kg)
global Gamma; Gamma=1.41; %specific heat ratio, cp/cv
global machr; machr = [0 0.2 0.3 0.4 0.5 0.6 0.8 0.9 0.95 1.05 1.1 ...
1.2 1.6 2.0 2.5 3 3.8 5 10 99];
global Cdr; Cdr =[.475475 .475475 .47576 .48336 .488965 .508345 ...
.56563 .618165 .668135 1.031795 1.01707 .990565 ...
.815955 .69236 .60971 .54606 .513 .494 .48317 .48317];
dtr=pi/180;
%Initial conditions:
long = -80.55*dtr;
%initial longitude.
lat = 28.5*dtr;
%initial latitude.
rad=R0;
%initial radius (m)
vel=0.0001;
%initial velocity (m/s)
fpa=90*dtr;
%initial f.p.a
chi=170*dtr; %initial heading angle (measured from north)
orbinit = [long; lat; rad; vel; fpa; chi];
[t,x]=ode45('lambert_rocket',[0 275],orbinit);
er=(x(size(x,1),3)-6378.14e3-hf)/1000 %final radial error (km)
edel=(x(size(x,1),2)-delf)/dtr %final latitude error (deg.)
elam=(x(size(x,1),1)-lamf)/dtr %final longitude error (deg.)
