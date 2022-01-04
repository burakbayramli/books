


function z = dl(uu,ww)

global beta


    z = 1 + beta*(3*uu.^2 + 2*ww.*uu + ww.^2.*uu);
