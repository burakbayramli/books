
% Scatter plot of the 2d solution compared with 1d radially symmetric solution
%
% First run code in this directory and in 1drad with same set of output times.  
%
% Use error2rad to also compute and plot the error for a particular Frame

for Frame=0:30
   PlotType=4;
   plotframe2
   qscat = q;
   rscat = r;
   cd 1drad
   plotstyle = 'r-';
   plotframe1
   x1d = x;
   q1d = q;
   axis([0 1.5 -.5 5])
   cd ..
   hold on
   plot(rscat,qscat','or')
   plot(x1d,q1d,'k')
   hold off
   query
   end

   
