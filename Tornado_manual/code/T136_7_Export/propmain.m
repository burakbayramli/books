function [ prop2 ] = propmain(geo,state)
%UNTITLED Summary of this function goes here


X=[];
Y=[];
Z=[];



prop2.gamma=[];

%[a b]=size(geo.prop.pos); %a=number of props
a=sum((geo.prop.n./geo.prop.n)); %a=number of props


i=1;
while i<=a;

    prop=propgen(geo.prop.n(i),geo.prop.dia(i),state.AS,geo.prop.rpm(i),state.rho,geo.prop.T(i),geo.prop.rot(i,1));

    X=[X; prop.XYZ(:,:,1)+geo.prop.pos(i,1)];
    Y=[Y; prop.XYZ(:,:,2)+geo.prop.pos(i,2)];
    Z=[Z; prop.XYZ(:,:,3)+geo.prop.pos(i,3)];
    prop2.gamma=[prop2.gamma; prop.gamma];
    
    if geo.prop.Psym(i)==1
          prop=propgen(geo.prop.n(i),geo.prop.dia(i),state.AS,geo.prop.rpm(i),state.rho,geo.prop.T(i),geo.prop.rot(i,1)*((geo.prop.Rsym(i)*2-1)));

          X=[X; prop.XYZ(:,:,1)+geo.prop.pos(i,1)];
          Y=[Y; prop.XYZ(:,:,2)-geo.prop.pos(i,2)];
          Z=[Z; prop.XYZ(:,:,3)+geo.prop.pos(i,3)];
          prop2.gamma=[prop2.gamma; prop.gamma];
          
    
    end
    i=i+1;   
end

prop2.XYZ(:,:,1)=X;
prop2.XYZ(:,:,2)=Y;
prop2.XYZ(:,:,3)=Z;
end

function [out]=propgen(n,D,V,rpm,rho,T,axis)

%Function to generate propeller slipstream



%%Input parameters, move to function header before release
%n              %[-]        Number of blades
%D              %[m]        Propeller diameter
%V              %[m/s]      Free stream velocity
%rpm            %[RPM]      Rotational speed
%rho            %[kg/m^3]   Air density
%T              %[N]        Thrust
%axis           %[-]        rotational axis, currently only in +- x
%direction

A=pi*D^2/4;     %[m^2]  Disc area;


%% Naive Relax wake
w=0.5*(-V+sqrt(V^2+(2*T/(A*rho))));     %Incremental airspeed at prop
a=0.5*(-1+sqrt(1+8*T/pi));
%disp('Line 21, propgen')

A3=A*(V+w)/(V+2*w);                    %Slipstream area far downstream, mass continuity.
r3=sqrt(A3/pi);                        %Radius of streamtube far downstream

SHRINK=2*r3/D ;

%% Thrust check

%THRUST=rho*A3*(V+2*w)*(2*w);    %OK -Can comment oot
%disp('Line 33, propgen')

%% Simulation settings
nelem=12;                   %Number of elements on a circle
wt=20;                      %Wake turns
far=1;                      %Propeller diameters until wake convergence;

te=nelem*wt;                %Total number of elements in one wake
phi_offset=axis*2*pi/n;     %Multiple Blade Offset
phi_start = axis*0;%         -pi/4/10*4;         %Start angle



%%
dphi=axis*2*pi/nelem;       %Angular change per wake element
dl=(1/(rpm/60)*V)/nelem;    %Longitudinal change per wake element 
r0=D/2;                     %Radius at propeller

I=round(far*D/dl);          % wake index where wake is relaxed

l(1)=0;
r(1)=r0;
for i=2:te+1;               %This part is approximative !!! - linear shrinking.
    l(i)=(i-1)*dl;
    if i<I
        r(i)=(1-i/I)*r0+(i/I)*r3; %Radius is shrinking linerely until I
    else
        r(i)=r3;            %Radius is at final radius
    end
    
    
end

sd=wt/l(end);                %winds per meter (spiral density) ((remember that there is more than one blade))

for i=1:n
  
    phi=[phi_start:dphi:axis*2*pi*wt+phi_start]+phi_offset*(i-1);
    phi(1);
    y(i,:)=r.*cos(phi);
    z(i,:)=r.*sin(phi);
    x(i,:)=l;
    
    
    x2(i,:)=[x(i,end) 0 x(i,:)];
    y2(i,:)=[0 0 y(i,:)];                   %Adding core vortex 
    z2(i,:)=[0 0 z(i,:)]; 
end

%% Initial wake generation complete




%%Initial gamma assessment
gamma_0=2*w/(n*sd);    %SOLENOID EQUATION

out.XYZ(:,:,1)=x2;
out.XYZ(:,:,2)=y2;
out.XYZ(:,:,3)=z2;
out.gamma(1:n)=gamma_0*axis;

end
