function z = EFR13_12f(x,y)
if norm([x y]-[0 .5],2)<.125
z=100;
else
z=0;
end