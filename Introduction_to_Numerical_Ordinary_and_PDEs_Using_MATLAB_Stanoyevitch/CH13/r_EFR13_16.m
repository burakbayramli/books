function r=r_EFR13_16(x,y)
%u-coefficient function for Neumann/Robin BC of
%EFR13.16.
if (y>=0&y<=2) & x<=2
    r=1;
elseif (y>=0&y<=2) & x>=4
    r=2;
else
    r=0;
end
