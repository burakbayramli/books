function riverReport(fname)
% riverReport  Compute and plot summary of river flow data
%
% Synopsis:  riverReport(fname)
%
% Input:     fname = (string) name of file containing flow data
%
% Output:    Printed summary statistics and plots of flow data vs. time

% --- Read data into working vectors 
D = load(fname);           %  Read data from file into D matrix
h = D(:,1);  f = D(:,2);   %  Copy data into hour and flow rate vectors
D = [];                    %  Free memory used by D matrix

% --- Compute daily total and weekly average flow rates
[day,flowPerDay]   = dailyAve(h,f);
[week,flowPerWeek] = weeklyAve(h,f);

% --- Plot flow rates
subplot(2,1,1)
plot(h,f,'b.','markerSize',2);
xlabel('day of the year');  ylabel('Hourly Flow rate,  CFS');
title(sprintf('Flow data from %s',fname));

midweek = 7*week - 3.5;    %  vector of days in the middle of the week
subplot(2,1,2)
plot(day,flowPerDay,'r-',midweek,flowPerWeek,'ko');
xlabel('day of the year');  ylabel('Flow rate,  CFS');
legend('daily','weekly');
