function riverReport2(fname)
% riverReport2  Compute and plot summary of river flow data.  Compatible
%               with Student version of MATLAB 5.
%
% Synopsis:  riverReport(fname)
%
% Input:     fname = (string) name of file containing flow data
%
% Output:    Printed summary statistics and plots of flow data vs. time
%
% Note:      This version of riverReport assumes the data file contains
%            a single column of numbers which are the flow rates.  The
%            hour vector is created after the data is read from the file
%            This gets around the memory limitation of the student version.
%            Data for this program is in gc87flow.cfs

% --- Load and verify data
data = load(fname);          %  Read data from file
[m,n] = size(data);
if n==1                 % Data for student version has only one column
  fprintf('Warning in riverReport:\n');
  fprintf('\tData in file %s has %d columns, not two\n\n',fname,n);
  f = data;
  n = length(f);
  h = (1:n)';
elseif n==2
  h = data(:,1);  f = data(:,2);   % hour and flow rate
else
  error(sprintf('Data in %s needs to be in one or two columns',fname));
end
data = [];                       % Free memory used by data matrix

% --- Compute daily total and weekly average flow rates
[day,flowPerDay]   = dailyAve(h,f);
[week,flowPerWeek] = weeklyAve(h,f);

% --- Plot flow rates
plot(h/24,f,'b.','markerSize',2);
hold on
midweek = 7*week - 3.5;    %  vector of days in the middle of the week
plot(day,flowPerDay,'r-',midweek,flowPerWeek,'ko--');
xlabel('day of the year');  ylabel('Flow rate,  CFS');
title(sprintf('Flow data from %s',fname));
legend('hourly','daily','weekly');
