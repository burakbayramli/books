function plot2d_strain_cross(x,s1,s2,azi,comp_is_positive)
%
% plot strain crosses assuming that s1 and s2 are the largest and smallest
% eigenvalues, and azi is the azimuth CW from north in degree to s1
%

nsym=200;
n=size(s1,1);% plot only 1000 symbols
dn=round(n/nsym)+1;
pselect=1:dn:n;



if(comp_is_positive)
        fe=2;se=1;
else
        fe=1;se=2;
end
            
scale=max((s1-s2)/2)*20;

headl=5; % head size in pixels
axis(axis);

dx=[s1(pselect).*cos(azi(pselect)) s1(pselect).*sin(azi(pselect)) ];
dx = dx/scale;
arrow(x(pselect,:),x(pselect,:)+dx,'Length',headl,'Ends',fe,'EdgeColor','b','FaceColor','b');
arrow(x(pselect,:),x(pselect,:)-dx,'Length',headl,'Ends',fe,'EdgeColor','b','FaceColor','b');

dx=[-s2(pselect).*cos(azi(pselect)+pi/2) -s2(pselect).*sin(azi(pselect)+pi/2) ];
dx=dx/scale;
arrow(x(pselect,:),x(pselect,:)+dx,'Length',headl,'Ends',se,'EdgeColor','r','FaceColor','r');
arrow(x(pselect,:),x(pselect,:)-dx,'Length',headl,'Ends',se,'EdgeColor','r','FaceColor','r');