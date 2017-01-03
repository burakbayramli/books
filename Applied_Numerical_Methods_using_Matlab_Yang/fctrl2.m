function m=fctrl2(n)
if n<=1, m=1;
 else m=n*fctrl2(n-1);
end