% plot_covar.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot RMS uncertainties  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
sat_nos = '0';
for ell=1:length(sat_set)
   sat_nos = strcat(sat_nos,[', ',num2str(sat_set(ell))]);
end;
sat_nos = sat_nos(4:length(sat_nos));
plot(time,gdop_out);
grid on;
xlabel('Time (sec)')
ylabel('GDOP');
title(['GDOP for Satellite Numbers ',sat_nos]);
disp('Press RETURN key to continue');
pause;

plot(time,Pminus_out(1,:),'b-',time,Pplus_out(1,:),'r:');
grid on;
xlabel('Time (sec)')
ylabel('RMS X Error [m]');
title(['RMS Predicted/Corrected X Error [',sat_nos,']']);
legend('Predicted','Corrected');
disp('Press RETURN key to continue');
pause;

plot(time,Pminus_out(2,:),'b-',time,Pplus_out(2,:),'r:');
grid on;
xlabel('Time (sec)')
ylabel('RMS Y Error [m]');
title(['RMS Predicted/Corrected Y Error [',sat_nos,']']);
legend('Predicted','Corrected');
disp('Press RETURN key to continue');
pause;

plot(time,Pminus_out(3,:),'b-',time,Pplus_out(3,:),'r:');
grid on;
xlabel('Time (sec)')
ylabel('RMS Z Error [m]');
title(['RMS Predicted/Corrected Z Error [',sat_nos,']']);
legend('Predicted','Corrected');
disp('Press RETURN key to continue');
pause;

plot(time,Pminus_out(4,:),'b-',time,Pplus_out(4,:),'r:');
grid on;
xlabel('Time (sec)')
ylabel('RMS Clock Bias Error [m]');
title(['RMS Predicted/Corrected Clock Bias Error [',sat_nos,']']);
legend('Predicted','Corrected');
disp('Press RETURN key to continue');
pause;

plot(time,Pminus_out(5,:),'b-',time,Pplus_out(5,:),'r:');
grid on;
xlabel('Time (sec)')
ylabel('RMS Clock Drift Error [m/s]');
title(['RMS Predicted/Corrected Clock Drift Error [',sat_nos,']']);
legend('Predicted','Corrected');
disp('Press RETURN key to continue');
pause;

close all;
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  

