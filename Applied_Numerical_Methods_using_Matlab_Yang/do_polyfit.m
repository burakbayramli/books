%do_polyfit
clear, clf
%x=[-3 -2 -1 0 1 2 3]; 
%y=[-0.2774  0.8958  -1.5651  3.4565  3.0601  4.8568  3.8982];
%xy=[x' y'];
%save xy1.dat xy /ascii
load xy1.dat
x=xy1(:,1); y=xy1(:,2);
xi=min(x)+[0:100]/100*(max(x)-min(x)); %intermediate points
for i=1:4
  subplot(220+i) 
  [th,err,yi]=polyfits(x,y,2*i-1,xi); err %LS
  %[th,err,yi]=polyfits(x,y,-(2*i-1),xi); err %LS with no constant term
  plot(x,y,'k*',xi,yi,'b:')
  %[th1,err1,yi]=polyfits(x+10,y,2*i-1,xi); err1 %LS
  %[th2,err2,yi]=polyfits(x,y+10,2*i-1,xi); err2 %LS
  %th,th1,th2
  for m=1:length(x), sigma(m)=abs(y(m))+0.1; end
  [th,errw,yi]=polyfits(x,y,2*i-1,xi,sigma); errw %WLS
  hold on, plot(x,y,'k*',xi,yi,'r:')
end