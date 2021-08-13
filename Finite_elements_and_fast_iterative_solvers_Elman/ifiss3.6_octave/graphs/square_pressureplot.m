function square_pressureplot(qmethod,sol1,sol2,xy,xyp,x,y,spc,fig)
%SQUARE_PRESSUREPLOT compares pressure data on square domain
%   square_pressureplot(qmethod,sol1,sol2,xy,xyp,x,y,spc,fig);
%   input
%          qmethod    mixed method 
%          sol1       first flow solution vector
%          sol2       second flow solution matrix    
%          xy         velocity nodal coordinate vector  
%          xyp        pressure nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          spc        uniform/nonuniform streamline switch 
%          fig        figure number
%
%   IFISS function: DJS; 24 September 2009.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy); nu=2*nvtx; np=length(xyp);
p1=sol1(nu+1:end); p2=sol2(nu+1:end);
%
%% plot pressure
if qmethod==2
   xx=x(1:2:end); yy=y(1:2:end);
elseif qmethod==3
   p1=p1(1:3:end); xx=x(1:end); yy=y(1:end);
   p2=p2(1:3:end); xx=x(1:end); yy=y(1:end);
else
   xx=x(1:end); yy=y(1:end); 
end
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(xx,yy);
xysol1 = griddata(xyp(:,1),xyp(:,2),p1,X,Y);
xysol2 = griddata(xyp(:,1),xyp(:,2),p2,X,Y);
figure(fig)
if spc==1,
   subplot(121),contour(X,Y,xysol1,50),axis('square')	
   title('pressure comparison')
   subplot(122),contour(X,Y,xysol2,50),axis('square')	
   title('isolines: uniform'); 
   if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
elseif spc==2,
   list=[-0.2:0.01:0.3]';
   subplot(121),contour(X,Y,xysol1,list),axis('square')
   title('pressure comparison')
   subplot(122),contour(X,Y,xysol2,list),axis('square')	
   title('isolines: selected'); 
   if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
end	
axis('off')
return
