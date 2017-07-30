function plot2d_strain_cross(x,s1,s2,azi,comp_is_positive)
%
% plot strain crosses assuming that s1 and s2 are the largest and smallest
% eigenvalues, and azi is the azimuth CW from north in degree to s1
%
% Input: x(npoints,2) locations of stress symbols
%        s1, s2 : largest extensional and largest compressional eigenvalues
%        azi: azimuth is the angle to the first major stress axis (most extensive)
%             in degrees clockwise from north (direction 2, y-axis)

azir=azi/180*pi;% convert to radians

nsym=200;
n=size(s1,1);% plot only nsym symbols
dn=round(n/nsym)+1;
pselect=1:dn:n;

if(comp_is_positive)
    fe=2;se=1;
else
    fe=1;se=2;
end
            
scale=max((s1-s2)/2)*20; % scale with max shear stress

headl=5; % head size in pixels
axis(axis);

dx=[s1(pselect).*cos(azir(pselect)) s1(pselect).*sin(azir(pselect)) ];
dx = dx/scale;
arrow(x(pselect,:),x(pselect,:)+dx,'Length',headl,'Ends',fe,'EdgeColor','b','FaceColor','b');
arrow(x(pselect,:),x(pselect,:)-dx,'Length',headl,'Ends',fe,'EdgeColor','b','FaceColor','b');

dx=[-s2(pselect).*cos(azir(pselect)+pi/2) -s2(pselect).*sin(azir(pselect)+pi/2) ];
dx=dx/scale;
arrow(x(pselect,:),x(pselect,:)+dx,'Length',headl,'Ends',se,'EdgeColor','r','FaceColor','r');
arrow(x(pselect,:),x(pselect,:)-dx,'Length',headl,'Ends',se,'EdgeColor','r','FaceColor','r');