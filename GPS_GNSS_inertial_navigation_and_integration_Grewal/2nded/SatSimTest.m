%
global RA PA
YUMAdata;
r = 20000e3/pi;;
radperdeg = pi/180;
Lat = 0;
Lon = 0;
Alt = 0;
framenumber = 0;
for t=0:360:60*60*24,
    close all;
    figure;
    axis off;
    axis equal;
    Az = 145;
    sA = sin(Az*pi/180);
    cA = cos(Az*pi/180);
    El = 30;
    sE = sin(El*pi/180);
    cE = cos(El*pi/180);
    view(Az,El);
    uv = [sA*cE;-cA*cE;sE];
    for latdeg=-89:2:89,
        lat   = latdeg*pi/180;
        slat0 = sin(lat);
        clat0 = cos(lat);
        clatp = cos(lat+radperdeg);
        slatp = sin(lat+radperdeg);
        clatm = cos(lat-radperdeg);
        slatm = sin(lat-radperdeg);
        for londeg=1:2:359,
            lon   = londeg*pi/180;
            slon0 = sin(lon);
            clon0 = cos(lon);
            up    = [clon0*clat0,slon0*clat0,slat0];
            shade = abs(up*uv)*[1,1,1];
            clonp = cos(lon+radperdeg);
            slonp = sin(lon+radperdeg);
            clonm = cos(lon-radperdeg);
            slonm = sin(lon-radperdeg);
            x = r*[clatm*clonm,clatm*clonp,clatp*clonp,clatp*clonm];
            y = r*[clatm*slonm,clatm*slonp,clatp*slonp,clatp*slonm];
            z = r*[slatm,slatm,slatp,slatp];
            patch(x,y,z,shade,'LineStyle','none','FaceAlpha',.6);
        end;
    end;
    hold on;
    [ECIsats,ECIantenna] = testSatSim(t,Lat,Lon,Alt);
    plot3(ECIantenna(1),ECIantenna(2),ECIantenna(3),'kx');
    [three,n] = size(ECIsats);
    for k=1:n,
        plot3(ECIsats(1,k),ECIsats(2,k),ECIsats(3,k),'ko');
    end;
    framenumber = framenumber + 1;
    axis equal;
    MovieFrame(framenumber) = getframe;
    pause(1);
end;
movie(MovieFrame);