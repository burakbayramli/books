%EASY14   This script plots 5 figures:
%    1 - position error in Easting and Northing (GPS only versus EGNOS corrected)
%    2 - time series coordinate errors in Easting, Northing and Upping (GPS only vs EGNOS corrected)
%    3 - Horizontal Stanford plot
%    4 - Vertical   Stanford plot
%    5 - Number of satellites used to compute EGNOS corrected position, HPL and VPL
%
% Additionally some statistics will be computed for each set

% written by Kostas Dragunas
% last edited: 2008 12 20

set(0,'DefaultTextFontName','Times');
set(0,'DefaultAxesFontName','Times');
set(0,'DefaultTextFontSize',16);

% load data
load easy14_data.mat

% Horizontal alert limit
HAL = 35;
% Vertical alert limits
VAL1 = 12;
VAL2 = 20;

% epoch count
num_epoch   = size(recpos_XYZ_GPS,1);
% epoch count in hours
epoch_count = sprintf('%.2f hours',num_epoch/3600);

% initialization
rec_pos_UTM = zeros(num_epoch,6);
HPL         = zeros(num_epoch,1);
VPL         = zeros(num_epoch,1);
sat_nr      = zeros(num_epoch,1);

% get coordinate errors
% for GPS only
rec_pos_UTM(:,1) = rec_pos_utm_gps(:,1)-ref_pos_UTM(1);
rec_pos_UTM(:,2) = rec_pos_utm_gps(:,2)-ref_pos_UTM(2);
rec_pos_UTM(:,3) = rec_pos_utm_gps(:,3)-ref_pos_UTM(3);
% for EGNOS corrected
rec_pos_UTM(:,4) = rec_pos_utm_egnos(:,1)-ref_pos_UTM(1);
rec_pos_UTM(:,5) = rec_pos_utm_egnos(:,2)-ref_pos_UTM(2);
rec_pos_UTM(:,6) = rec_pos_utm_egnos(:,3)-ref_pos_UTM(3);

% compute HPE for EGNOS data
HPE(:,1) = (rec_pos_UTM(:,4).^2 + rec_pos_UTM(:,5).^2).^0.5;
% get VPE for EGNOS data
VPE(:,1) = rec_pos_UTM(:,6);

% adding sigmas
for i=1:num_epoch
    index = find(mask_EGNOS(i,:));
    sat_nr(i,1) = size(index,2);
    sigmas_all = sigma_EGNOS_flt(i,index) + sigma_EGNOS_iono(i,index)...
                             + sigma_EGNOS_tropo(i,index) + sigma_EGNOS_air(i,index);
    % compute HPL and VPL
    [HPL(i,1) cc VPL(i,1)] = SBAS_xPL(el_EGNOS(i,index),az_EGNOS(i,index),sigmas_all);
end

% -------------------------  Plot figures -----------------------------------------   
% get screen size parameters
fullscreen = get(0,'ScreenSize');

% ----------------------- Plot 1: Position error ----------------------------------
% position plot
% set figure name and position
figure('Name',['1. Position error,  ' epoch_count],'NumberTitle','off','Position',[0 60 fullscreen(3) fullscreen(4)-150]);
% plot GPS only receiver position
plot(rec_pos_UTM(:,1),rec_pos_UTM(:,2),'k*');
hold on
% plot EGNOS corrected receiver position
plot(rec_pos_UTM(:,4),rec_pos_UTM(:,5),'r*');
% set axes labels, title and legend
xlabel('East error [m]')
ylabel('North error [m]')
title(['Position error,  ' epoch_count])
legend('GPS without correction','EGNOS corrected','Location','NorthEast','Orientation','horizontal')
% set focus range
xlim([-6 6])
ylim([-6 6])
% make x and y axis equal
set(gca,'DataAspectRatio',[6 6 6]);
% control grid
grid on
set(gca,'XTick',[-6 -4 -2 -0 2 4 6]);
set(gca,'YTick',[-6 -4 -2 -0 2 4 6]);
hold off

print -depsc2 easy14a

% ----------------------- Plot 2: Position error versus time ----------------------------------
% time series plot
% set figure name and position
figure('Name',['2. Position error versus time, ' epoch_count],'NumberTitle','off','Position',[0 60 fullscreen(3) fullscreen(4)-150])
% east 
subplot(3,1,1)
% GPS only
plot(1:size(rec_pos_UTM,1),rec_pos_UTM(:,1),'k');
hold on
% EGNOS corrected
plot(1:size(rec_pos_UTM,1),rec_pos_UTM(:,4),'r');
% set axes, labels, title, and legend
title(['Position error versus time, ' epoch_count])
ylabel('East error [m]')
legend('GPS without correction','EGNOS corrected','Location','NorthEast','Orientation','horizontal')
% set focus range
ylim([-7 7])
xlim([0 num_epoch])
% control grid
grid on
set(gca,'XTick',[0 3600 7200 10800 14400]);
set(gca,'XTickLabel',{'0';'1h';'2h';'3h';'4h'});
set(gca,'YTick',[-5 -2.5 -0 2.5 5]);
hold off

% north
subplot(3,1,2)
% GPS only
plot(1:size(rec_pos_UTM,1),rec_pos_UTM(:,2),'k');
hold on
% EGNOS corrected
plot(1:size(rec_pos_UTM,1),rec_pos_UTM(:,5),'r');
% set axes, labels
ylabel('North error [m]')
% set focus range
ylim([-7 7])
xlim([0 num_epoch])
% control grid
grid on
set(gca,'XTick',[0 3600 7200 10800 14400]);
set(gca,'XTickLabel',{'0';'1h';'2h';'3h';'4h'});
set(gca,'YTick',[-5 -2.5 0 2.5 5]);
hold off

% up
subplot(3,1,3)
% GPS only
plot(1:size(rec_pos_UTM,1),rec_pos_UTM(:,3),'k');
hold on
% EGNOS corrected
plot(1:size(rec_pos_UTM,1),rec_pos_UTM(:,6),'r');
% set axes, labels
xlabel('Time [h]')
ylabel('Up error [m]')
% set focus range
ylim([-5 25])
xlim([0 num_epoch])
% control grid
grid on
set(gca,'XTick',[0 3600 7200 10800 14400]);
set(gca,'XTickLabel',{'0';'1h';'2h';'3h';'4h'});
set(gca,'YTick',[0 5 10 15 20 25]);
hold off

print -depsc2 easy14b

% ----------------------- Plot 3: Horizontal Stanford plot ----------------------------------
% time series plot
% set figure name and position
figure('Name',['3. Horizontal Stanford plot - ' epoch_count],'NumberTitle','off','Position',[0 60 fullscreen(3) fullscreen(4)-150]);
% the function will do all the plotting 
hplstat(HPL',HPE,HAL,'EGNOS');
% make x and y axis equal
set(gca,'DataAspectRatio',[10 10 10]);
hold off

print -depsc2 easy14c

% ----------------------- Plot 4: Vertical Stanford plot ----------------------------------
% set figure name and position
figure('Name',['4. Vertical Stanford plot - ' epoch_count],'NumberTitle','off','Position',[0 60 fullscreen(3) fullscreen(4)-150]);
% the function will do all the plotting 
vplstat(VPL',VPE,VAL1,VAL2,'EGNOS');
% make x and y axis equal
set(gca,'DataAspectRatio',[10 10 10]);
hold off

print -depsc2 easy14d

% ----------------------- Plot 5: Number of satellites used for position computation -----------
% set figure name and position
figure('Name',['5. Number of satellites used - ' epoch_count],'NumberTitle','off','Position',[0 60 fullscreen(3) fullscreen(4)-150])
plot(1:size(sat_nr,1),sat_nr,'b');
% set axes labels and title
xlabel('Time [h]')
ylabel('Number of satellites')
title(['Number of satellites used for position computation (EGNOS) - ' epoch_count])
% set focus range
ylim([0 12])
xlim([0 num_epoch])
% control grid
grid on
set(gca,'XTick',[0 3600 7200 10800 14400]);
set(gca,'XTickLabel',{'0';'1h';'2h';'3h';'4h'});
set(gca,'YTick',[0 5 7 9 11]);

% --------------------------------------- end of plotting -------------------------------------  

% print some statistics
fprintf('-------------------------------- Data: %s -----------------------------------\n',epoch_count);
fprintf('|        Time from: %d-%02d-%02d %02d:%02d:%.2f  ===>> to: %d-%02d-%02d %02d:%02d:%.2f         |\n',...
    start_x.year,start_x.month,start_x.day,start_x.hour,start_x.minute,start_x.second,...
    end_x.year,end_x.month,end_x.day,end_x.hour,end_x.minute,end_x.second);
fprintf('-------------------------------------------------------------------------------------\n');
fprintf('\n                 CEP    CEP    1DRMS    2DRMS    std east  std north  mean    mean \n');
fprintf('                (50%%)  (95%%)   (~63%%)   (~98%%)  (1 sigma) (1 sigma)   east    north\n');
fprintf('-------------------------------------------------------------------------------------\n');

% Compute CEP (50%), CEP (95%), 1DRMS, 2DRMS, RMS (1 sigma)
% standard deviation for easting and northing
EGNOS_std    = [ std(rec_pos_UTM(:,4))      std(rec_pos_UTM(:,5))];  % EGNOS
GPS_std      = [ std(rec_pos_UTM(:,1))      std(rec_pos_UTM(:,2))];  % EGNOS
% CEP 50%
EGNOS_CEP1   = 0.589 * (EGNOS_std(1) + EGNOS_std(2));
GPS_CEP1     = 0.589 * (GPS_std(1) + GPS_std(2));
% CEP 95%
EGNOS_CEP2   = EGNOS_CEP1*2.08;
GPS_CEP2     = GPS_CEP1*2.08;
% 1DRMS
EGNOS_DRMS1  = norm(EGNOS_std);
GPS_DRMS1    = norm(GPS_std);
% 2DRMS
EGNOS_DRMS2  = EGNOS_DRMS1*2;
GPS_DRMS2    = GPS_DRMS1*2;
% mean
EGNOS_east   = mean(rec_pos_UTM(:,4));
GPS_east     = mean(rec_pos_UTM(:,1));
EGNOS_north  = mean(rec_pos_UTM(:,5));
GPS_north    = mean(rec_pos_UTM(:,2));

statx = [GPS_CEP1,GPS_CEP2,GPS_DRMS1,GPS_DRMS2,GPS_std(1),GPS_std(2),GPS_east,GPS_north;
    EGNOS_CEP1,EGNOS_CEP2,EGNOS_DRMS1,EGNOS_DRMS2,EGNOS_std(1),EGNOS_std(2),EGNOS_east,EGNOS_north ];

fprintf('GPS only        %.1f     %.1f     %.1f      %.1f       %.1f       %.1f      %.1f     %.1f\n',statx(1,:));
fprintf('EGNOS           %.1f     %.1f     %.1f      %.1f       %.1f       %.1f      %.1f     %.1f\n\n',statx(2,:));

% clear temp variables
clear statx GPS_CEP1 GPS_CEP2 GPS_DRMS1 GPS_DRMS2 GPS_std GPS_east GPS_north
clear EGNOS_CEP1 EGNOS_CEP2 EGNOS_DRMS1 EGNOS_DRMS2 EGNOS_std EGNOS_east EGNOS_north
clear HAL VAL VPL HPL HPE VPE VAL1 VAL2 HAL
clear cc epoch_count i index num_epoch rec_pos_UTM sat_nr sigmas_all fullscreen

%%%%%%%%%%%%%%%%%%%%%%  end easy14.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%%