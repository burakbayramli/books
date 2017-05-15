%Example of box plots

data=[1 5 8 3;
      3 2 1 5;
      5 4 8 1;
      9 12 1 3;
      14 0 2 2;
      7 9 1 3];
  
median(data) %median of each column
mean(data)  %mean of each column 
std (data)  % standard deviation of each column

figure(1)
boxplot(data)
title('box plot example')
  
