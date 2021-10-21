
% print wave speeds:

pl = .4*(data(1,3) - .5*data(1,2)^2/data(1,1));
ul = data(1,2)/data(1,1)  % contact speed
cl = sqrt(1.4*pl/data(1,1));
ulmcl = ul-cl   % 1-wave speed
ss = (data(mx,2)-data(1,2)) / (data(mx,1)-data(1,1))  % shock speed
