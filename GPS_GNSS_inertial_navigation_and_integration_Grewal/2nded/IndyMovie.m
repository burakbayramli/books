%
% Make movie of racecar trajectory on an oval track similar to one
% at Indianapolis International Speedway.
%
% This particular trajectory maximizes the radius in turns.
% It follows the outside walls in the straightaways
% and just kisses the inside wall in the middle of turns.
%
% Width of track assumed 50 feet in straightaways, 60' in turns.
% Added 10' of width assumed to be on the inside of turns.
%
% The plot is rotated 90 degrees counter-clockwise to better
% fill the plotting area.  (North will be to the left.)
%
%
turnradius        = 5280/(2*pi); % turning radius for 1/4 mile length
longstraightaway  = 3300; % [ft]
shortstraightaway = 660; % [ft]
width             = shortstraightaway + 2*turnradius;
length            = longstraightaway + 2*turnradius;
halfwidth         = width/2;
halflength        = length/2;
close all;
figure;
fill([-3000,3000,3000,-3000],[-2350,-2350,2350,2350],'k');
axis equal;
hold on;
plot([-longstraightaway/2,longstraightaway/2],[-halfwidth,-halfwidth],'w:');
plot([-longstraightaway/2,longstraightaway/2],[halfwidth,halfwidth],'w:');
plot([-halflength,-halflength],[-shortstraightaway/2,shortstraightaway/2],'w:');
plot([halflength,halflength],[-shortstraightaway/2,shortstraightaway/2],'w:');
%
plot(longstraightaway/2+(turnradius-10)*sin((0:90)*pi/180),shortstraightaway/2+(turnradius-10)*cos((0:90)*pi/180),'w:');
plot(longstraightaway/2+(turnradius-10)*sin((90:180)*pi/180),-shortstraightaway/2+(turnradius-10)*cos((90:180)*pi/180),'w:');
plot(-longstraightaway/2+(turnradius-10)*sin((180:270)*pi/180),-shortstraightaway/2+(turnradius-10)*cos((180:270)*pi/180),'w:');
plot(-longstraightaway/2+(turnradius-10)*sin((270:360)*pi/180),shortstraightaway/2+(turnradius-10)*cos((270:360)*pi/180),'w:');
%
plot([-longstraightaway/2,longstraightaway/2],[-halfwidth-50,-halfwidth-50],'w:');
plot([-longstraightaway/2,longstraightaway/2],[halfwidth+50,halfwidth+50],'w:');
plot([-halflength-50,-halflength-50],[-shortstraightaway/2,shortstraightaway/2],'w:');
plot([halflength+50,halflength+50],[-shortstraightaway/2,shortstraightaway/2],'w:');
%
plot(longstraightaway/2+(turnradius+50)*sin((0:90)*pi/180),shortstraightaway/2+(turnradius+50)*cos((0:90)*pi/180),'w:');
plot(longstraightaway/2+(turnradius+50)*sin((90:180)*pi/180),-shortstraightaway/2+(turnradius+50)*cos((90:180)*pi/180),'w:');
plot(-longstraightaway/2+(turnradius+50)*sin((180:270)*pi/180),-shortstraightaway/2+(turnradius+50)*cos((180:270)*pi/180),'w:');
plot(-longstraightaway/2+(turnradius+50)*sin((270:360)*pi/180),shortstraightaway/2+(turnradius+50)*cos((270:360)*pi/180),'w:');
%
x           = 60/(sqrt(2)-1);
plot([-longstraightaway/2+x,longstraightaway/2-x],[-halfwidth-50,-halfwidth-50],'w-');
plot([-longstraightaway/2+x,longstraightaway/2-x],[halfwidth+50,halfwidth+50],'w-');
plot([-halflength-50,-halflength-50],[-shortstraightaway/2+x,shortstraightaway/2-x],'w-');
plot([halflength+50,halflength+50],[-shortstraightaway/2+x,shortstraightaway/2-x],'w-');
%
curveradius = turnradius - 10 + sqrt(2)*x;
plot(longstraightaway/2-x+curveradius*sin((0:90)*pi/180),shortstraightaway/2-x+curveradius*cos((0:90)*pi/180),'w-');
plot(longstraightaway/2-x+curveradius*sin((90:180)*pi/180),-shortstraightaway/2+x+curveradius*cos((90:180)*pi/180),'w-');
plot(-longstraightaway/2+x+curveradius*sin((180:270)*pi/180),-shortstraightaway/2+x+curveradius*cos((180:270)*pi/180),'w-');
plot(-longstraightaway/2+x+curveradius*sin((270:360)*pi/180),shortstraightaway/2-x+curveradius*cos((270:360)*pi/180),'w-');
tracklength = 2*pi*curveradius + 2*(longstraightaway-2*x) + 2*(shortstraightaway-2*x);
hold off;
