%do_wlse2
clear, clf
x=[1:2:20]; Nx=length(x); %changing input
xi=[1:200]/10; %interpolation points
eb=0.4*ones(size(x)); %error bound for each y
y=[4.7334 2.1873 3.0067 1.4273 1.7787 1.2301 1.6052 1.5353 ... 
1.3985 2.0211];
[x,i]=sort(x); y=y(i); eb=eb(i); %sort the data for plotting
eby=y.*eb; %our estimation of error bounds
KC=6; [thlc,err,yl]=curve_fit(x,y,KC,0,xi); thlc
[thwl,err,ywl]=curve_fit(x,y,KC,0,xi,eby); thwl
f=inline('th(1)*x.^th(2)','th','x');
[th,err]=lsqcurvefit(f,[0 0],x,y)
yi1=f(th,xi);
errorbar(x,y,eby), hold on
plot(xi,yl,'b', xi,ywl,'r', xi,yi1,'m:')
