%do_wlse1 for Ex.3.7
clear, clf
x=[1 3 5 7 9 2 4 6 8 10]; %input data
y=[0.0831 0.9290 2.4932 4.9292 7.9605 ...
   0.9536 2.4836 3.4173 6.3903 10.2443]; %output data
eb=[0.2*ones(5,1); ones(5,1)]; %error bound for each y
[x,i]=sort(x); y=y(i); eb=eb(i); %sort the data for plotting
errorbar(x,y,eb,':'), hold on
N=2; %the degree of the approximate polynomial
xi=[0:100]/10; %interpolation points
[thl,errl,yl]=polyfits(x,y,N,xi); 
[thwl,errw,ywl]=polyfits(x,y,N,xi,eb);
thl, thwl
KC=0; thlc=curve_fit(x,y,KC,N,xi) %for cross-check
thwlc=curve_fit(x,y,KC,N,xi,eb)
f=inline('th(1)*x.^2 +th(2)*x+th(3)','th','x');
[th,err]=lsqcurvefit(f,[0 0 0],x,y)
yi1=f(th,xi);
plot(xi,yl,'b', xi,ywl,'r', xi,yi1,'m:')

