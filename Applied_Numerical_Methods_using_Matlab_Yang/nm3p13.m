%nm3p13.m
tho=[1 2]; %true parameter
x=ones(1,10); %the unchanged input
y=tho(1)*x+tho(2)+(rand(size(x))-0.5);
th_ls=polyfits(x,y,1); %uses the MATLAB routine in Sec.3.8-2
polyfit(x,y,1) %uses MATLAB built-in function
