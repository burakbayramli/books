% easyplot:  Script to plot data in file xy.dat

%  Load the data
D = load('xy.dat');       %  D is matrix with two columns
x = D(:,1);  y = D(:,2);  %  x in 1st column, y in 2nd column

plot(x,y)      %  Generate the plot and label it
xlabel('x axis, unknown units')
ylabel('y axis, unknown units')
title('plot of generic x-y data set')
