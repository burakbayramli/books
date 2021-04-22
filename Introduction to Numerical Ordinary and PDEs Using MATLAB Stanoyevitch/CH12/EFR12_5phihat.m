function y = EFR12_5phihat(x)
if (x>=0)&(x<=10), y=BSSpline(x-3);
elseif (x<0)&(x>=-10), y = -EFR12_5phihat(-x);
else q=floor((x+10)/20);, y=EFR12_5phihat(x-20*q);
end
