%EASY11    Creates a stereographic plot of GPS satellite
%          orbits from an almanac. The plot is as seen
%          from the position (phi,lambda). All orbits
%          with elevation angle lower than a given
%          cut-off angle (mask) are omitted.
%          Almanac files most easily can be downloaded
%          from your own (high-end) receiver or
%          http://www.ngs.noaa.gov/CORS/Data.thml
%
%          An additional plot is created showing number
%          of visible satellites and when they are visible

%Kai Borre 26-08-2008
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2008/08/26  $

set(0,'DefaultTextFontName','Times');
set(0,'DefaultAxesFontName','Times');
set(0,'DefaultTextFontSize',16);

% the following five assignments are necessary
rinexe('brdc1550.08n','aau.nav');
almanac = 'aau.nav';
mask = 10;
phi = [57 0 0];
lambda = [10 0 0];

%reading ephemerides
fide = fopen(almanac,'r');
Eph = fread(fide,inf,'double');
m = length(Eph);
eph = reshape(Eph,21,m/21);

% transformation of given location (phi,lambda,h) to (X,Y,Z)
Phi = dms2rad(phi(1),phi(2),phi(3));
Phi = Phi*180/pi;
Lambda = dms2rad(lambda(1),lambda(2),lambda(3));
Lambda = Lambda*180/pi;
[M(1,1),M(2,1),M(3,1)] = frgeod(6378137,298.257222101,Phi,Lambda,0);

% Computation of (azimuth, elevation) for each satellite
% for each 15 min.s. We use only one ephemeris for each PRN.
% Anyway, the plot is only for visual use
[prns, ind] = unique(eph(1,:));
az = ones(32,96)*inf;
el = ones(32,96)*inf;

for sat = [ind]
    start_time = eph(18,sat);
    j = 0;
    i = eph(1,sat);
    for time = start_time:900:start_time+86400
        S = satpos(time,eph(:,sat));
        j = j+1;
        [azimuth,elevation,distance] = topocent(M,S-M);
        az(i,j) = azimuth;
        el(i,j) = elevation;
    end
end

%%%%%%%%% stereographic plot  %%%%%%%%%%%%%
figure(1);
% polarhg draws coordinate lines of a polar plot. We add
% circles with radii 30 and 60 degrees
polarhg([30 60])
XX = zeros(32,40)*inf; % a matrix of NaNs to store plot data
YY = XX;

hold on
for k = 1:32
    if az(k,1) == 0, continue, end
    AZ = az(k,:);
    EL = el(k,:);
    % remove data below the cut-off angle
    AZ(find(EL <= mask)) = nan; 
    EL(find(EL <= mask)) = nan;
    % convertion to polar coordinates
    xx = (90-EL).*cos(AZ*pi/180);
    yy = (90-EL).*sin(AZ*pi/180);
    XX(k,1:length(xx)) = xx;
    YY(k,1:length(yy)) = yy;
end % end-k
% the first coord. moves text vertically (increasing values up),
% the second coord. moves text horizontally (increasing values right)
text(135,-95,{['Skyplot for the position (\phi, \lambda) = (' ...
    num2str(round(Phi)) '\circ, '  num2str(round(Lambda)) '\circ)']})
text(115,-45,{['Elevation mask  ' num2str(mask) '\circ' ]}) %120
text(-120,-120,['All PRNs except  ' num2str(setdiff(1:32,prns)) ])
plot(XX',YY','linewidth',2)
hold off

print -depsc2 easy111

break

% preparation for visibility plot  %%%%%%%%%%%%%%%%%

% we choose a resolution of 5 min.s,
% ie. 24 hours times 12 = 288 which becomes the range of j
satsum = zeros(1,288);
visible = zeros(2*(size(prns,2)+1),288);

for sat = [ind]
    Az = [];
    El = [];
    i = eph(1,sat);
    for j = 1:288
        time = 300*(j-1);
        S = satpos(time,eph(:,sat));
        [az,el,d] = topocent(M,S-M);
        if el > mask
            Az = [Az az];
            El = [El el];
            satsum(j) = satsum(j)+1;
            visible(2*i,j) = 1;
        end
    end
end

figure(2);
set(gca,'Fontsize',16);
area(satsum)
set(gca,'XTick',1:71:288)
set(gca,'XTickLabel',{'0','6','12','18','24'})
xlabel('GPS Time [hours]')
ylabel('# of Visible Satellites')
title(['Elevation Mask ' num2str(mask) '\circ'])

print -depsc2 easy112

figure(3);
set(gca,'Fontsize',16);
imagesc(flipud(visible)); colormap(gray)
set(gca,'XTick',1:71:288)
set(gca,'XTickLabel',{'0','6','12','18','24'})
set(gca,'YTick',-3:16:(2*(size(prns,2)+1)))
set(gca,'YTickLabel',{'2','8','16','24','32'});
xlabel('GPS Time [hours]')
ylabel('PRNs')
title('Solid Lines Indicate Visible Satellites')
colormap summer

print -depsc2 easy113
%%%%%%%%%%%%%%%%%%%%% end easy11.m %%%%%%%%%%%%%%
