function z=h_EFR13_16(x,y)
%Inhomogeneity function for Neumann/Robin BC of
%EFR13.16.  
if y>2+eps & y<6-eps
    z=0;
elseif y>=6-eps
    z=20;
elseif y<eps
    z=-20;
elseif (y>=eps & y<=2+eps)|[x y]==[2 0]|[x y]==[4 0]
    z=30;
end
if y==0 & (x==2|x==4)
    z=5;
end
if y==2
    z=15;
end
if y==6 % (x==2|x==4)
    z=5;
end

    