% Plots the difference between batch processing,
% Kalman filter and extended Kalman filter

%Kai Borre 04-06-97
%Copyright (c) 1997
%$Revision: 1.0 $  $Date: 1997/09/22  $

e = exist('offset.eps');
if e ~= 0
   delete offset.eps
end

offset_batch = recclock('08100761.94o','fjellera.nav');
offset_kalman = kalclock('08100761.94o','fjellera.nav',0);
offset_kal_ext = kalclock('08100761.94o','fjellera.nav',1);

figure;
hold on
plot(offset_batch,'g-')
plot(offset_kalman,'b--')
plot(offset_kal_ext,'r:')
%title('Receiver clock offset','Fontsize', 18)
xlabel('Epochs, interval 15 s','Fontsize', 16);
ylabel('Clock offset [ns]','Fontsize', 16);
set(gca,'Fontsize', 16);
legend('Batch processing', 'Kalman filter', 'Extended Kalman filter')
hold off
print offset -deps
%%%%%%%%%%%%%%%%% offset.m  %%%%%%%%%%%%%%%%%%%%%%%%%%
