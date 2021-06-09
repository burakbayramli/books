function integ=triangquad2d(fun,v1,v2,v3)
% M-file for EFR 13.8.  This function will integrate a function
% of two variables x and y over a triangle T in the plane. 
% It uses the M-file 'quad2d' of Program 13.1
% Input variables:  fun = a symbolic expression (using one or both of the
% symbolic variables x and y,  v1, v2, and v3:  three length 2 vectors
% giving the vertices of the triangle T in the plane.
% NOTE: Before this program is used, x and y should be declared as symbolic
% variables.
syms x y u
vys = [v1(2) v2(2) v3(2)];
vxs = [v1(1) v2(1) v3(1)];
minx=min(vxs);, maxx=max(vxs);
miny=min(vys);
minyind =find(vys==miny);
minxind =find(vxs==minx);
maxxind =find(vxs==maxx);
if length(minxind)==2|length(maxxind)==2 %triangle has a vertical side
    if length(minxind)==2
        vertx=minx;
        vertymax=max(vys(minxind));
        vertymin=min(vys(minxind));
    else 
        vertx=maxx;
        vertymax=max(vys(maxxind));
        vertymin=min(vys(maxxind));
    end
    thirdind = find(vxs~= vertx);
    topslope=sym((vys(thirdind)-vertymax)/(vxs(thirdind)-vertx));
    botslope=sym((vys(thirdind)-vertymin)/(vxs(thirdind)-vertx));
    ytop=topslope*(x-vxs(thirdind))+vys(thirdind);
    ylow=botslope*(x-vxs(thirdind))+vys(thirdind);
    integ = quad2d(fun,minx,maxx,ylow,ytop);
else %no vertical sides so vertices have 3 different x coordinates
    midind = find(vxs>minx& vxs<maxx); %index of middle vertex
    longslope=sym((vys(maxxind)-vys(minxind))/(maxx-minx));
    ylong = longslope*(x-vxs(minxind))+vys(minxind);
   if vys(midind)>subs(ylong,x,vxs(midind));%long edge lies below mid vertex
     topleftslope = sym((vys(midind)-vys(minxind))/(vxs(midind)-vxs(minxind))); 
     toprgtslope = sym((vys(midind)-vys(maxxind))/(vxs(midind)-vxs(maxxind)));
     ytopleft = topleftslope*(x-vxs(midind))+vys(midind);
     ytoprgt = toprgtslope*(x-vxs(midind))+vys(midind);
     integ = quad2d(fun,minx, vxs(midind),ylong,ytopleft)+...
         quad2d(fun,vxs(midind),maxx,ylong,ytoprgt);
   else %long edge lies above mid vertex
     botleftslope = sym((vys(midind)-vys(minxind))/(vxs(midind)-vxs(minxind)));
     botrgtslope = sym((vys(midind)-vys(maxxind))/(vxs(midind)-vxs(maxxind))); 
     ybotleft = botleftslope*(x-vxs(midind))+vys(midind);
     ybotrgt = botrgtslope*(x-vxs(midind))+vys(midind);
     integ = quad2d(fun,minx,vxs(midind),ybotleft,ylong)+...
         quad2d(fun,vxs(midind),maxx,ybotrgt,ylong);
   end
end

     
        