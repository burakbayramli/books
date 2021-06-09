function y = EX12_2phihat(x)
if (0 <= x)&(x <= 3), y=x/3;
elseif (x >=3)&(x<=4), y=4-x;
elseif (x<0)&(x>=-4), y = -EX12_2phihat(-x);
else q=floor((x+4)/8);, y=EX12_2phihat(x-8*q);
end