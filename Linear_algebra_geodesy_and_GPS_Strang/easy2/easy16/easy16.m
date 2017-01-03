%EASY16            The script plots figures showing
%                  Changes in (X,Y,Z)
%                  Changes in (E,N)
%                  Changes in U
%                  Elevation angle to satellite
%                  Ionospheric delay
%                  Tropshperic delay
%                  Multipath
%                  Differences between positions computed from 
%                      broadcast and precise ephemerides
%                  Differences projected onto line-of-sight
%             for data collected on January 19, 2007 by Topcon
%             receivers and stored in Gril format

%Kai Borre 11-07-2008
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2008/07/11  $

set(0,'DefaultTextFontName','Times');
set(0,'DefaultAxesFontName','Times');
set(0,'DefaultTextFontSize',16);

global EPH fidr

% Initial computations of constants
v_light = 299792458; 	     % vacuum speed of light m/s
f1 = 154*10.23E6;		     % L1 frequency Hz
f2 = 120*10.23E6;		     % L2 frequency Hz
alpha1 = f1^2/(f1^2-f2^2);   % for use in ionospheric-free comb.
alpha2 = -f2^2/(f1^2-f2^2);
alpha = (f1/f2)^2;
A = [1 1 0 0;
    1 -1 v_light/f1 0;
    1 alpha 0 0;
    1 -alpha 0 v_light/f2];
invA = inv(A);

% You may manually SELECT one of the following PRNs: 3, 11, 14, 18, 19,22
% 28:
PRN = 14;

EPH = zeros(21,32);
% storage of the various issues of the ephemerides for the selected PRN
EPH_acc = [];

%masterfile = '19jan07m.log';
%fidr = fopen(masterfile);
roverfile =  '19jan07r.log';
fidr = fopen(roverfile);

Pos = [];
elv = [];
I = [];
T = [];
M1 = [];

figure(1);
set(gcf,'UserData',zeros(3,1)*inf);
pp = plot(1,zeros(3,1)*inf,'.',...
    'Erasemode','None','MarkerSize',1);
set(pp(1),'Color','green','marker','o')
set(pp(2),'Color','blue')
set(pp(3),'Color','green')
ylabel('Changes in Position Over Time [m]')
xlabel('Epochs  [interval 1 s]')
scale = 15;     % value chosen for this data set
epochs = 2536;  % the given data files contain 2536 epochs
axis([0 epochs -scale scale]);
figdata = [];
EPH_acc = [EPH_acc EPH(:,PRN)]; % we include a first column of zeros
ii = 2;

for kk = 1:epochs
    ii = ii+1;
    old_toe = EPH(21,PRN);
    [tow,prn,P1,Phi1,P2,Phi2] = readGril;
    if old_toe ~=  EPH(21,PRN)
        EPH_acc = [EPH_acc EPH(:,PRN)];
        old_toe;
    end
    if kk == 1, towb = tow; end

    % In the obervation file any missing observation was
    % substituted by an NaN; now we test if NaN is present and
    % delete the corresponding PRN and pertinent data
    mis_obs = isnan(P1);
    if sum(mis_obs) > 0
        fprintf('\nObservation Deleted in Epoch %4.0f ',tow);
        col = find(mis_obs == 1);
        prn(:,col) = [];
        P1(:,col) = [];
        Phi1(:,col) = [];
        P2(:,col) = [];
        Phi2(:,col) = [];
    end

    prnE = find(EPH(1,:) > 0);
    [good, iprn, iprnE] = intersect(prn,prnE);
    lg = length(good);
    if lg >= 4,
        % re-ordering all useful observations in
        % increasing order
        P1good = [];
        Phi1good = [];
        P2good = [];
        Phi2good = [];
        for m = 1:lg
            P1good = [P1good; P1(iprn(m))];
            Phi1good = [Phi1good; Phi1(iprn(m))];
            P2good = [P2good; P2(iprn(m))];
            Phi2good = [Phi2good; Phi2(iprn(m))];
        end

        % remove measurements with NaNs
        good(find(isnan(P1good)==1)) = [];
        P1good(find(isnan(P1good)==1)) = [];
        lg = length(good);

        if lg >= 4
            [pos, El] = recposRTK(tow,good,P1good);
            if kk >= 4
                % set reference position only if it is available and only once
                if isempty(Pos)==1
                    Pos = pos;
                end
                set(pp,'XData',(ii-2)*ones(3,1),'YData',pos(1:3,1)-Pos(1:3,1))
                drawnow
                figdata = [figdata pos(1:3,:)];
            end  %kk
        end  %lg
    end %lg
      j = find(good == PRN);
    % computation of multipath
    m1 = P1good(j)-(alpha+1)/(alpha-1)*Phi1good(j)+2/(alpha-1)*Phi2good(j);
    M1 = [M1 m1];
    % computation of ionospheric delay
    % I can be computed from pseudoranges alone, but the result is very noisy
    % ion = (P1good(j)-P2good(j))/(1-alpha);
    % In the first epoch we derived I from a matrix equation; however, this implies
    % the estimation of N1 = x(3) and N2 = x(4).    
    if kk == 1
       x = invA*[P1good(j); Phi1good(j); P2good(j); Phi2good(j)]%;
    end
    ion = (-v_light/f1*x(3)+v_light/f2*x(4) + Phi1good(j)-Phi2good(j))/(alpha-1);
    I = [I ion];
    % computation of tropospheric delay
    el = El(j);
    elv = [elv el];
    t = tropo(sin(el*pi/180),0,1013,260,50,0,0,0);
    T = [T t];
end  %kk
fprintf('\n');

% Next, we prepare for a comparison between broadcast and precise ephemerides
% First computation of satellite positions from broadcast ephemeris
broad_X = [];
EPH_acc(:,1) = []; % we delete the first column of zeros
timeb = towb;      % first epoch in seconds
timee = tow;       % last epoch
j1 = 1;
for i = timeb:60:timee
    if i > EPH_acc(21,j1)
        j1 = 2;
    end
    X(1:3,1) = satpos(i,EPH_acc(:,j1));
    broad_X = [broad_X X];
end

% We read the precise orbit positions file which is found at
% http://igscb.jpl.nasa.gov/igscb/product/1408/
fid = fopen('igr14105.sp3','r');
for j = 1:22  % skipping the header of the SP3 file
    fgetl(fid);
end

% The receiver is switched on on January 19, 2007
% we want to find the corresponding Julian Day Number
jd = julday(2007,1,19,13.85);
jdp = 0;

line = fgetl(fid);
% We find an epoch (modulo 15 minutes) 60 minutes ahead of jd.
% jd is counted in unit of day
while jdp < jd-1/24
    [star,rest] = strtok(line);
    [years,rest] = strtok(rest); year = str2num(years);
    [months,rest] =strtok(rest); month = str2num(months);
    [days,rest] = strtok(rest); day = str2num(days);
    [hours,rest] = strtok(rest); hour = str2num(hours);
    [mins,rest] = strtok(rest); min = str2num(mins);
    [secs,rest] = strtok(rest); sec = str2num(secs);
    jdp = julday(year,month,day,hour+min/60+sec/3600);
    while 1
        line = fgetl(fid);
        if strtok(line) == '*', break, end
    end
end

t2 = 0;
line = fgetl(fid);
while 1
    [p,rest] = strtok(line);
    if p == ['PG' num2str(PRN)]    
        t2 = t2+1;
        [x,rest] = strtok(rest); Xp(1,t2) = 1000*str2num(x);
        [y,rest] = strtok(rest); Xp(2,t2) = 1000*str2num(y);
        [z,rest] = strtok(rest); Xp(3,t2) = 1000*str2num(z); 
        %[t,rest] = strtok(rest); Xp(4,t2) = 1000*str2num(t);
        % sat.clock offset in nanoseconds
    end
    line = fgetl(fid);
    if strtok(line) == '*', line = fgetl(fid); end
    % We read precise orbits for the interpolation period plus
    % three times 15 minutes ahead of the period and three
    % times 15 minutes after the period
    if t2 == ceil((timee-timeb)/900)+6, break, end
end
fclose all;

%The matrix Xp contains the coordinates of the selected PRN for 
%epochs: 13:15 hours, 13:30 hours, ... , 15:15 hours, in total 
%9 = bas sets
bas = size(Xp,2); 
%Precise ephemerides are issued every 15 minutes, starting at 
%the whole hour. In the follwoing we use an epoch scale in units of 15
%minutes. This could as well be chosen in whole minutes.
%We have timeb = 13:51 = 3.85 quarters of an hour, and timee = 
%14:33.2... = 6.65 ... quarters of an hour. The entire observation 
%period is 2.8 quartes of an hour, which again is dt = 43 [minutes]
%indicating the integer ceiled number of minutes for which we compute 
%positions based on the precise ephemerides
dt = ceil((timee-timeb)/60); 
%We want an interpolated position each minute, hence we divide by 15 
%to keep units in quartes of an hour. The first parameter in the intp 
%procedure describes the abscissae for the points at which we know the 
%satellite coordinates. The second parameter contains these coordinates,
%and the third parameter describes the point set at which we want 
%interpolated values, all in units of quarter of an hour
for tt = 1:dt
    POSp(1,tt) = intp([1:bas]',Xp(1,:)',3+(6+(tt-1))/15);
    POSp(2,tt) = intp([1:bas]',Xp(2,:)',3+(6+(tt-1))/15);
    POSp(3,tt) = intp([1:bas]',Xp(3,:)',3+(6+(tt-1))/15);
end
% coordinate differences between broadcast and precise ephemerides
y = broad_X-POSp;
for k = 1:size(y,2)
    yy(1,k) = norm(y(1:3,k));
end
% The actual influence of the orbit error on the receiver position
% is given by the projection of the difference vector onto the line
% between receiver and satellite. The latter given by X and the
% receiver position pos.
for j = 1:length(y)
    a = broad_X(:,j)-pos(1:3); % direction between receiver and satellite
    b = y(:,j);            % difference vector between broadcast and precise eph
    p = (a'*b)/(a'*a)*a;   % projected difference vector
    np(j) = norm(p);
    if a'*b < 0, np(j) = -np(j); end % we include a sign
end


% Now we start plotting all generated data
figure(2);
% In order to make this figure to show the change in position for all
% epochs one has to add 'default' positions in figdata.
% for example this would be the case if ephemeris are not in the first epoch:
xxx = [ones(1,kk-3-size(figdata,2))*Pos(1); ...
    ones(1,kk-3-size(figdata,2))*Pos(2); ...
    ones(1,kk-3-size(figdata,2))*Pos(3)];
figdata = [xxx figdata];
plot(1:kk-3,figdata-figdata(:,1)*ones(1,kk-3));
% and this is the case where we draw only the data which is available,
% however we loose those epochs in the plot where data was missing and
% thus, we can not compare this plot 1 to 1 (e.g. rover and master)
plot(1:size(figdata,2),figdata-figdata(:,1)*ones(1,size(figdata,2)))
ylabel('Change in Position [m]','fontsize',16)
xlabel('Epochs  [1 s]','fontsize',16)
legend('\itX','\itY','\itZ')
set(gca,'fontsize',16)
num = size(figdata,2);
for i = 1:num
    [N,E,U] = cart2utm(figdata(1,i),figdata(2,i),figdata(3,i),32);
    n(i) = N;
    e(i) = E;
    u(i) = U;
end
print -depsc2 easy1602

figure(3);
plot(n-ones(1,num)*n(1),e-ones(1,num)*e(1),'+')
title(['Changes in ({\itN, \itE}) over ' num2str(num) ' s'],'fontsize',16)
xlabel('{\itE}  [m]','fontsize',16)
ylabel('{\itN}  [m]','fontsize',16)
set(gca,'fontsize',16)
axis equal
print -depsc2 easy1603

figure(4);
plot(u-ones(1,num)*u(1))
xlabel('Epochs [interval 1 s]','fontsize',16)
ylabel('Changes in height {\itU}  [m]','fontsize',16)
set(gca,'fontsize',16)
print -depsc2 easy1604

% Elevation angle
figure(5);
plot(elv,'linewidth',1)
xlabel('Epochs  [interval 1 s]','fontsize',16)
ylabel('Elevation angle [{\it\circ}]','fontsize',16)
set(gca,'fontsize',16)
%print -depsc2 easy1605
print -deps2 easy1605


%Ionosphere
figure(6);
plot(I,'linewidth',1)
xlabel('Epochs  [interval 1 s]','fontsize',16)
ylabel('Ionospheric delay {\itI} [m]','fontsize',16)
set(gca,'fontsize',16)
%print -depsc2 easy1606
print -deps2 easy1606


%Troposphere
figure(7);
plot(T,'linewidth',1)
xlabel('Epochs  [interval 1 s]','fontsize',16)
ylabel('Tropospheric delay {\itT} [m]','fontsize',16)
set(gca,'fontsize',16)
%print -depsc2 easy1607
print -deps2 easy1607


%Multipath
figure(8);
plot(M1-M1(1),'linewidth',.8)
xlabel('Epochs [interval 1 s]','fontsize',16)
ylabel('Multipath {\itM} [m]','fontsize',16)
set(gca,'fontsize',16)
%print -depsc2 easy1608
print -deps2 easy1608


%Position differences between broadcast and precise ephemerides
figure(9);
yl = 1:size(y,2);
plot(yl,y(1,:)','o',yl,y(2,:)','x',yl,y(3,:)','+','Markersize',3)
xlabel('Time [minutes]','FontSize',16)
str1(1) = {'Difference between broadcast and precise ephemerides'};
str1(2) = {'Vector components ({\itX}, {\itY}, {\itZ}) [m]'};
str1(3) = {'Norm of difference vector marked by blue stars [m]'};
ylabel(str1,'VerticalAlignment','Bottom','FontSize',14)
hold on
plot(yy,'*','MarkerSize',4)
hold off
set(gca,'FontSize',16)
legend('\itX','\itY','\itZ','||  ||')  % \mid\mid
print -depsc2 easy1609
%print -deps2 easy1609

%Position differenecs projected onto line of sight; this projection
%is the measured error on the pseudorange
figure(10);
%plot(np,'o','MarkerSize',3)
plot(np,'*','MarkerSize',4)
xlabel('Time [minutes]','FontSize',16)
str2(1) = {'Difference in radial direction'};
str2(2) = {'between broadcast and precise ephemerides [m]'};
ylabel(str2,'VerticalAlignment','Bottom','FontSize',14)
set(gca,'FontSize',16)
print -depsc2 easy1610
%print -deps2 easy1610


%%%%%%%%%%%%%%%%%%%%%%%%%%  easy16.m  %%%%%%%%%%%
