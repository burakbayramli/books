function PSI = transfer_bcs(Y0,Yf)
global r0bar u0bar v0bar ufbar theta0
PSI = [Y0(1)-r0bar
       Y0(2)-u0bar
       Y0(3)-v0bar
       Y0(4)-theta0
       Yf(2)-ufbar
       Yf(3)-sqrt(1/Yf(1))
       -Yf(5)+1/2*Yf(7)/Yf(1)^(3/2)-1];% Final Condition
return
