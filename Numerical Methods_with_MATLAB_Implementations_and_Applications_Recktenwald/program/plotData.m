function plotData(fname)
% plotData   Plot (x,y) data from columns of an external file
%
% Synopsis:  plotData(fname)
%
% Input:     fname = (string) name, including extension, of the 
%                    file containing data to be plotted
%
% Output:    A plot in a separate figure window

data = load(fname);    %  load contents of file into data matrix  
x = data(:,1);         %  x and y are in first two columns of data
y = data(:,2);
plot(x,y,'o');
