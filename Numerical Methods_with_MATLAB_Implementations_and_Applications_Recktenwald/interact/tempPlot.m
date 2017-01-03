% tempPlot.m  Plot monthly temperature variations for Portland, OR.
%             Along the way demonstrate plot annotation commands

load pdxTemp.dat;         %  Read data into pdxTemp matrix 
month = pdxTemp(:,1);     %  Copy first column of PDXTemp into month
temps = pdxTemp(:,2:4);   %  And remaining columns into temps

%  create plot with different symbols for each data set
plot(month,temps(:,1),'ro',month,temps(:,2),'k+',month,temps(:,3),'b-');

xlabel('Month');                      % add axis labels and plot title
ylabel('Temperature (degrees F)');
title('Monthly average temperature for Portland International Airport');
axis([1 12 20 100]);
legend('High','Low','Average');     %  add legends
