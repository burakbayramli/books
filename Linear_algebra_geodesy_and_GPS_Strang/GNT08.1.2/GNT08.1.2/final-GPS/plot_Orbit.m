%This Function show the satellite and its orbit+Earth geometry 
%CopyRight By Moein Mehrtash
%**************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/28/2008              *
% Email: moeinmehrtash@yahoo.com                                          *
%**************************************************************************
%**************************************************************************
%Plot Orbit+Erath
%Function's Inputs:
    %A:(m)          =>semi-major orbit axis
    %ec:            =>eccentricity
    %Inc:(Rad)      =>Inclination angle
    %Omega:(Rad)    =>Longitude of Ascending Node
    %v:(Rad)        =>True Anomaly
    %Color:(string) =>The color of orbit
    %Name:(string)  =>Name of satellite
%Function's Output:
    %Plot Orbit $ Satellite
    %Plot Earth
%**************************************************************************
function plot_Orbit(Orbit_parameter,color)

[m,n]=size(Orbit_parameter);

     E=Orbit_parameter(2,:);
     A=Orbit_parameter(9,:);
     ec=Orbit_parameter(10,:);
     Inc=Orbit_parameter(7,:);
     Omega=Orbit_parameter(8,:);
     v=Orbit_parameter(3,:);

for i=1:n
     ii=num2str(i);
     Name_SV=strcat('SV-',ii);
     
Nu=0:2*pi/100:2*pi;
r=A(i)*(1-ec(i)^2)./(1+ec(i).*cos(Nu));
x = r.*cos(Nu);
y = r.*sin(Nu);

xs=x.*cos(Omega(i))-y.*cos(Inc(i)).*sin(Omega(i));
ys=x.*sin(Omega(i))+y.*cos(Inc(i)).*cos(Omega(i));
zs=y.*sin(Inc(i));
plot3(xs,ys,zs,color(i),'Linestyle','-')

hold on
xlabel('Xaxis')
ylabel('Yaxis')
zlabel('Zaxis')



r0=A(i)*(1-ec(i)^2)/(1+ec(i)*cos(v(i)));
x0 = r0*cos(v(i));
y0 = r0*sin(v(i));

xp=x0*cos(Omega(i))-y0*cos(Inc(i))*sin(Omega(i));
yp=x0*sin(Omega(i))+y0*cos(Inc(i))*cos(Omega(i));
zp=y0*sin(Inc(i));

plot3(xp,yp,zp,'blacko','Linewidth',2); % plot true anomaly
text(xp+.1*xp,yp+.1*yp,zp+.1*zp,Name_SV);


axis_data = get(gca);
xmin = axis_data.XLim(1);
xmax = axis_data.XLim(2);
ymin = axis_data.YLim(1);
ymax = axis_data.YLim(2);
zmin = axis_data.ZLim(1);
zmax = axis_data.ZLim(2);

% I, J ,K vectors
 R=6399592;
plot3([0,2*R],[0 0],[0 0],'black','Linewidth',2); plot3(2*R,0,0,'black>','Linewidth',2.5);
plot3([0 0],[0,2*R],[0 0],'black','Linewidth',2); plot3(0,2*R,0,'black>','Linewidth',2.5);
plot3([0 0],[0 0],[0,2*R],'black','Linewidth',2); plot3(0,0,2*R,'black^','Linewidth',2.5);

% right ascending node line plot
xomega_max = xmax*cos(Omega(i)*pi/180);
xomega_min = xmin*cos(Omega(i)*pi/180);
yomega_max = ymax*sin(Omega(i)*pi/180);
yomega_min = ymin*sin(Omega(i)*pi/180);

xlabel('I');
ylabel('J');
zlabel('K');

 R=6399592;
    [x,y,z] = ellipsoid(0,0,0,R,R,R,20);
     surfl(x, y, z)
     shading faceted %interp
     colormap(gray);
    hold on
  az = 120;
   el = 30;
   view(az, el);
end