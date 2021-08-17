function [TRI,xout,yout,uout,interp] = PlotField2D(Nout, xin, yin, uin)

% function [TRI,xout,yout,uout,interp] = PlotField2D(Nout, xin, yin, uin)
% Purpose: filled contour plot of solution data

Globals2D;
     
% build equally spaced grid on reference triangle
Npout = (Nout+1)*(Nout+2)/2;
rout = zeros(Npout,1); sout = zeros(Npout,1); 
sk = 1;
for n=1:Nout+1
  for m=1:Nout+2-n
    rout(sk) = -1 + 2*(m-1)/Nout;
    sout(sk) = -1 + 2*(n-1)/Nout;
    counter(n,m) = sk; sk = sk+1;
  end
end

% build matrix to interpolate field data to equally spaced nodes
interp = InterpMatrix2D(rout, sout);

% build triangulation of equally spaced nodes on reference triangle
tri = []; 
for n=1:Nout+1
  for m=1:Nout+1-n,
    v1 = counter(n,m);   v2 = counter(n,m+1); 
    v3 = counter(n+1,m); v4 = counter(n+1,m+1);
    if(v4) 
      tri = [tri;[[v1 v2 v3];[v2 v4 v3]]]; 
    else
      tri = [tri;[[v1 v2 v3]]]; 
    end
  end
end

% build triangulation for all equally spaced nodes on all elements
TRI = [];
for k=1:K
  TRI = [TRI; tri+(k-1)*Npout];
end

% interpolate node coordinates and field to equally spaced nodes
xout = interp*xin; yout = interp*yin; uout = interp*uin;

% render and format solution field
trisurf(TRI, xout(:), yout(:), uout(:));
shading interp,    material shiny,    lighting gouraud 
camlight headlight
return
