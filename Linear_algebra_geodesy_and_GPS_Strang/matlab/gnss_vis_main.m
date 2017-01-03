% GNSS Visibility Assignment
% FILE NAME: GNSS_VIS_MAIN.m
% DESCRIPTION: to run run_vis and produce plots of satellite visibility
% INPUT: user_input
% OUTPUT: vis (struct)
% Last modify: 2007-09-22   by: paloma.farias@colorado.edu
 
clc;
clear all;
close all;
 
%Run main function 
vis = run_vis ('user_input');
 
%________________________________________________________________________
%(Q.3)Plot visible satellites constalltion
 
% %make arr for PRN
 prn_nm = zeros((vis.data(end,vis.col.PRN)),1);
 
% %find PRN
 prn_nm = vis.data(find(vis.data(:,vis.col.TOW) == vis.data(1,vis.col.TOW)), vis.col.PRN);
 
% %convert PRN to string
 prn_str =num2str(prn_nm); 
 
%plot all sat 
figure(1)
scatter(vis.rightass, vis.arglat,'MarkerEdgeColor','k','MarkerFaceColor','g');
text(vis.rightass+5, vis.arglat,prn_str);
title ('GPS constellation for gps week 419');
xlabel('Right Ascension of the Ascending Node [Deg]');
ylabel('Arguments of Latitude [Deg]');
grid
%________________________________________________________________________
 
%(Q5) Plot all sats regardless if visible or not
% example code from lecture #4 and #5
 
%a) Plot AZ and EL  0N 0E, (all sat fig 2, only visible sat fig 3)
figure(2)
plot_azel(vis.data(:, vis.col.AZ), vis.data(:, vis.col.EL), [])
title ('All Satellites at 0N 0E')
 
i = find(vis.data(:, vis.col.EL) > 0);
figure(3)
%plot_azel(vis.data(i, vis.col.AZ), vis.data(i, vis.col.EL),[])
title ('Only Visible Satellites at 0N 0E ')
 
% %only plot visible sats at last time step: routine used for comparison
% with garmin
lasttime = find(vis.data(i,vis.col.TOW) == vis.data(end,vis.col.TOW));
figure
plot_azel(vis.data(i(lasttime), vis.col.AZ), vis.data(i(lasttime), vis.col.EL), vis.data(i(lasttime), vis.col.PRN))
vis.data(i(lasttime), vis.col.PRN)
title ('Only Visible Satellites at 40N 105W - 23 sep. 21.10 (24 Sep. 4.10 UTC)')
 
% %b) Plot AZ and EL  90N 0E, (all sat fig 4, only visible sat fig 5)
% figure(4)
% plot_azel(vis.data(:, vis.col.AZ), vis.data(:, vis.col.EL), [])
% title ('All Satellites at 90N 0E')
% 
% i = find(vis.data(:, vis.col.EL) > 0);
% figure(5)
% plot_azel(vis.data(i, vis.col.AZ), vis.data(i, vis.col.EL), [])
% title ('Only Visible Satellites at 90N 0E')
 
% %c) Plot AZ and EL  40N 105E, (all sat fig 6, only visible sat fig 7)
% figure(6)
% plot_azel(vis.data(:, vis.col.AZ), vis.data(:, vis.col.EL), [])
% title ('All Satellites at 40N, 105W')
% 
% i = find(vis.data(:, vis.col.EL) > 0);
% figure(7)
% plot_azel(vis.data(i, vis.col.AZ), vis.data(i, vis.col.EL), [])
% title ('Only Visible Satellites at 40N, 105W ')
%________________________________________________________________________
% (Q6) compute difference of AZ and EL 10 and 100 km away from Boulder
y = size(vis.data(:,vis.col.AZ));
AZ_av = sum(vis.data(:,vis.col.AZ))/(y(1,1)-1);
EL_av = sum(vis.data(:,vis.col.EL))/(y(1,1)-1);
%________________________________________________________________________
% (Q7) Period calculations (semi major axis a from YUMA in meters)
G = 6.67259e-11; %Nm^2/kg^2  Gravitational const.
M= 5.9736e24; % kg Earth mass
 
PRN1_T = (2*pi)/(sqrt((G*M)/(5153.359863^2)^3)) 
PRN2_T = (2*pi)/(sqrt((G*M)/(5153.563477^2)^3))
PRN3_T = (2*pi)/(sqrt((G*M)/(5153.568359^2)^3))
 
PRN1_O = (2*pi*5153.359863^2); 
PRN2_O = (2*pi*5153.563477^2);
PRN3_O = (2*pi*5153.568359^2);
 
%________________________________________________________________________
%(Q9) Calculate PDOPs
tms = find( vis.data(:,vis.col.TOW) == vis.data(1,vis.col.TOW));
tms_nm = length(find(vis.data(:,vis.col.PRN) == vis.data(i(1),vis.col.PRN)));
 
for tms_in=1:tms_nm
    % Find visible sat at current time
    sat = find(vis.data(tms,vis.col.EL) > vis.mask.EL);
    
    % call function PDOPcal to calculate PDOP
    [sat_nm(tms_in) pdop(tms_in)] = PDOPcal( vis.data(tms(sat),vis.col.AZ),vis.data(tms(sat),vis.col.EL));
                                          
    % get pdops times 
    pdop_tms(tms_in) = vis.data(tms(sat(1)),vis.col.TOW); 
    tms = tms+1;
end
 
% Plot PDOP
figure()
[AX,H1,H2]= plotyy(pdop_tms, pdop,pdop_tms, sat_nm,'plot');
[AX,H1,H2]= plotyy(pdop_tms, pdop,pdop_tms, sat_nm,'plot');
 
title('Position of Dilution of Precision PDOP') 
xlabel('Time Of Week (GPS wk. 419)')
 


