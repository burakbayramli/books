function [xs,ys,xys] = stretch_grid(x,y,xy,bndxy,bnde,sbnde,h)
%STRETCH_GRID generates stretched grid
%   [xs,ys,xys] = stretch_grid(x,y,xy,bndxy,bnde,sbnde,h)
%   input
%          x          x coordinate vector
%          y          y coordinate vector
%          xy         coordinates of nodes
%          bndxy      boundary coordinates
%          bnde       boundary edges
%          sbnde      boundary edges near which refinement is needed
%          h          mesh size
%   output
%          xs         stretched x coordinate
%          ys         stretched y coordinate
%          xys        new coordinates of nodes
%
% IFISS function: HCE; 28 December 2009. 
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
% Code written by M. Wu, 2009
% 28 Dec 2009:  currently only used for obstacle, called by obstacle_domain

xs = x; ys = y; xys = xy;

ne = size(bnde,1);
nse = length(sbnde);
he = []; ve = []; hey = []; vex = [];
hse = []; vse = []; hsey = []; vsex = [];

%% sort out the horizontal and vertical edges 
for i = 1:ne
    x1 = bndxy(bnde(i,1),1); x2 = bndxy(bnde(i,2),1);
    y1 = bndxy(bnde(i,1),2); y2 = bndxy(bnde(i,2),2);
    if x1 == x2
        ve = [ve;bnde(i,1:2)]; vex = [vex;x1];
    elseif y1 == y2
        he = [he;bnde(i,1:2)]; hey = [hey;y1];
    else
        error('Incorrect edge data!')
    end
end
[s,IX] = sort(vex); ve = ve(IX,:);
[s,IX] = sort(hey); he = he(IX,:);

for i = 1:nse
    x1 = bndxy(bnde(sbnde(i),1),1); x2 = bndxy(bnde(sbnde(i),2),1);
    y1 = bndxy(bnde(sbnde(i),1),2); y2 = bndxy(bnde(sbnde(i),2),2);
    if x1 == x2
        vse = [vse;bnde(sbnde(i),1:2)]; vsex = [vsex;x1];
    elseif y1 == y2
        hse = [hse;bnde(sbnde(i),1:2)]; hsey = [hsey;y1];
    else
        error('Incorrect edge data!')
    end
end
[s,IX] = sort(vsex); vse = vse(IX,:);
[s,IX] = sort(hsey); hse = hse(IX,:);

%% vertical stretch
nhe = size(he,1);
nhse = size(hse,1);
for i = 1:nhse
    if i == 1
        ix = find(he(:,1)==hse(i,1) & he(:,2)==hse(i,2));
        if ix>1
            k = ix - 1;
            yd = bndxy(he(k,1),2); yu = bndxy(hse(i,1),2);
            while yd == yu && k>1
                k = k - 1;
                yd = bndxy(he(k,1),2);
            end
               if yd~=yu
                   ixu = find(y == yu); ixd = find(y == yd);
                   N = (yu - yd)/h; nc = log(2*N)/log(2);
                   y3 = yd + nc*h/2; y4 = yd;
                   ny2 = 1; ny3 = N - ny2;
                   ynew = subint_contract(yu,y3,y4,ny2,ny3);
                   ys(ixd:ixu) = ynew;
                   for j = ixd:ixu
                       ix = find(xy(:,2) == y(j));
                       xys(ix,2) = ys(j);
                   end
               end
        end
    else
           yd = bndxy(hse(i-1,1),2); yu = bndxy(hse(i,1),2); 
           if yd~=yu
              ym = (yd + yu)/2;
              ixu = find(y == yu); ixd = find(y == yd);
              N = (yu - yd)/h; nc = log(N)/log(2);
              y1 = yd; y2 = ym - nc*h/2; y3 = ym + nc*h/2; y4 = yu;
              ny1 = N/2 - 1; ny2 = 2; ny3 = N/2 - 1;
              ynew = subint(y1,y2,y3,y4,ny1,ny2,ny3);
              ys(ixd:ixu) = ynew;
              for j = ixd:ixu
                  ix = find(xy(:,2) == y(j));
                  xys(ix,2) = ys(j);
              end
           end
    end
    if i==nhse
        ix = find(he(:,1)==hse(i,1) & he(:,2)==hse(i,2));
        if ix<nhe
            k = ix + 1;
            yd = bndxy(hse(i,1),2); yu = bndxy(he(k,1),2);
            while yd==yu && k<nhe
                k = k + 1;
                yu = bndxy(he(k,1),2);
            end
               if yd~=yu
                  ixu = find(y == yu); ixd = find(y == yd);
                  N = (yu - yd)/h; nc = log(2*N)/log(2);
                  y3 = yu - nc*h/2; y4 = yd;
                  ny2 = 1; ny3 = N - ny2;
                  ynew = subint_expand(yu,y3,y4,ny3,ny2);
                  ys(ixd:ixu) = ynew;
                  for j = ixd:ixu
                      ix = find(xy(:,2) == y(j));
                      xys(ix,2) = ys(j);
                  end
               end
        end
    end        
end

%% horizontal stretch
nve = size(ve,1);
nvse = size(vse,1);
for i = 1:nvse
    if i == 1
        ix = find(ve(:,1)==vse(i,1) & ve(:,2)==vse(i,2));
        if ix>1
            k = ix - 1;
            xl = bndxy(ve(k,1),1); xr = bndxy(vse(i,1),1);
            while xl == xr && k>1
                k = k - 1;
                xl = bndxy(ve(k,1),1);
            end
               if xl~=xr
                   ixr = find(x == xr); ixl = find(x == xl);
                   N = (xr - xl)/h; nc = log(2*N)/log(2);
                   x3 = xl + nc*h/2; x4 = xl;
                   nx2 = 1; nx3 = N - nx2;
                   xnew = subint_contract(xr,x3,x4,nx2,nx3);
                   xs(ixl:ixr) = xnew;
                   for j = ixl:ixr
                       ix = find(xy(:,1) == x(j));
                       xys(ix,1) = xs(j);
                   end
               end
        end
    else
           xl = bndxy(vse(i-1,1),1); xr = bndxy(vse(i,1),1); 
           if xl~=xr
              xm = (xl + xr)/2;
              ixr = find(x == xr); ixl = find(x == xl);
              N = (xr - xl)/h; nc = log(N)/log(2);
              x1 = xl; x2 = xm - nc*h/2; x3 = xm + nc*h/2; x4 = xr;
              nx1 = N/2 - 1; nx2 = 2; nx3 = N/2 - 1;
              xnew = subint(x1,x2,x3,x4,nx1,nx2,nx3);
              xs(ixl:ixr) = xnew;
              for j = ixl:ixr
                  ix = find(xy(:,1) == x(j));
                  xys(ix,1) = xs(j);
              end
           end
    end
    if i==nvse
        ix = find(ve(:,1)==vse(i,1) & ve(:,2)==vse(i,2));
        if ix<nve
            k = ix + 1;
            xl = bndxy(vse(i,1),1); xr = bndxy(ve(k,1),1);
            while xl==xr && k<nve
                k = k + 1;
                xr = bndxy(ve(k,1),1);
            end
               if xl~=xr
                  ixr = find(x == xr); ixl = find(x == xl);
                  N = (xr - xl)/h; nc = log(2*N)/log(2);
                  x3 = xr - nc*h/2; x4 = xl;
                  nx2 = 1; nx3 = N - nx2;
                  xnew = subint_expand(xr,x3,x4,nx3,nx2);
                  xs(ixl:ixr) = xnew;
                  for j = ixl:ixr
                      ix = find(xy(:,1) == x(j));
                      xys(ix,1) = xs(j);
                  end
               end
        end
    end        
end

%% fix the coordinates of boundary nodes
for i = 1:nhe
    yb = bndxy(he(i,1),2);
    ix = find(xy(:,2) == yb);
    xys(ix,2) = yb;
    ix = find(y==yb);
    ys(ix) = yb;
end

for i = 1:nve
    xb = bndxy(ve(i,1),1);
    ix = find(xy(:,1) == xb);
    xys(ix,1) = xb;
    ix = find(x==xb);
    xs(ix) = xb;
end

return
