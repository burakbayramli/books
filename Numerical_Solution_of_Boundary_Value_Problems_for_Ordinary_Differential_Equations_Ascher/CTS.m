function [x,y] = CTS
% Import solution of CTS.f90

temp = load('CTS.dat');
TP = temp(1,1);
NSUB = temp(1,2);
epsilon = temp(2,1);
x = temp(3:end,1);
y = temp(3:end,2);

plot(x,y,'r','LineWidth',2)
if TP == 7
    title(['y''(x) with \epsilon = ',num2str(epsilon),...
           ' and ',num2str(NSUB),' subintervals.'])
    xlabel('Test problem 7. Corner layer near 0 is clearer in a plot of y''(x).')  
elseif TP == 23
   title(['y(x) with \mu = ',num2str(1/epsilon),...
          ' and ',num2str(NSUB),' subintervals.'])
   xlabel(['Test problem ',num2str(TP),'.'])    
elseif TP == 31
   title(['M(x) with \epsilon = ',num2str(epsilon),...
          ' and ',num2str(NSUB),' subintervals.'])
   xlabel(['Test problem ',num2str(TP),'.'])       
elseif TP == 32
   title(['y''(x) with R = ',num2str(1/epsilon),...
          ' and ',num2str(NSUB),' subintervals.'])
   xlabel(['Test problem ',num2str(TP),'.'])       
else
   title(['y(x) with \epsilon = ',num2str(epsilon),...
          ' and ',num2str(NSUB),' subintervals.'])
   xlabel(['Test problem ',num2str(TP),'.'])   
end

