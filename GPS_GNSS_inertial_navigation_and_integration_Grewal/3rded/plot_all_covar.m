% plot_all_ covar.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot RMS uncertainties  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
sat_nos = '0';
for ell=1:length(sat_set)
   sat_nos = strcat(sat_nos,[', ',num2str(sat_set(ell))]);
end;
sat_nos = sat_nos(4:length(sat_nos));
semilogy(time,gdop_out,'k-');
grid on;
xlabel('Time (sec)')
ylabel('GDOP');
title('GDOP for Prob 10.7');
print -dbmp -r600 'plot_all_covar_fig01.bmp';
figure;%pause;

semilogy(time,Pminus_out(1,:),'k-',time,Pplus_out(1,:),'k-.');
grid on;
xlabel('Time (sec)')
ylabel('RMS X Error [m]');
title('RMS Predicted/Corrected X Error [Prob 10.7]');
legend('Predicted','Corrected');
print -dbmp -r600 'plot_all_covar_fig02.bmp';
figure;%pause;

semilogy(time,Pminus_out(2,:),'k-',time,Pplus_out(2,:),'k-.');
grid on;
xlabel('Time (sec)')
ylabel('RMS Y Error [m]');
title('RMS Predicted/Corrected Y Error [Prob 10.7]');
legend('Predicted','Corrected');
print -dbmp -r600 'plot_all_covar_fig03.bmp';
figure;%pause;

semilogy(time,Pminus_out(3,:),'k-',time,Pplus_out(3,:),'k-.');
grid on;
xlabel('Time (sec)')
ylabel('RMS Z Error [m]');
title('RMS Predicted/Corrected Z Error [Prob 10.7]');
legend('Predicted','Corrected');
print -dbmp -r600 'plot_all_covar_fig04.bmp';
figure;%pause;

semilogy(time,Pminus_out(4,:),'k-',time,Pplus_out(4,:),'k-.');
grid on;
xlabel('Time (sec)')
ylabel('RMS Clock Bias Error [m]');
title('RMS Predicted/Corrected Clock Bias Error [Prob 10.7]');
legend('Predicted','Corrected');
print -dbmp -r600 'plot_all_covar_fig05.bmp';
figure;

semilogy(time,Pminus_out(5,:),'k-',time,Pplus_out(5,:),'k-.');
grid on;
xlabel('Time (sec)')
ylabel('RMS Clock Drift Error [m/s]');
title('RMS Predicted/Corrected Clock Drift Error [Prob 10.7]');
legend('Predicted','Corrected');
print -dbmp -r600 'plot_all_covar_fig06.bmp';
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  

