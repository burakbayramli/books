function node = node48(Nx,Ny,x0,xf,y0,yf)
%Step3: Assigning global coordinates to nodes 
%Nx=Number of elements in x-directions
%Ny=Number of elements in y direction
%Lx=Length of rectangular domain in x dir
%Ly=Length of rectangular domain in y dir
%node4=coordinates of 4 node elements
%node8=coordinates of 8 node elements
dx=(xf-x0)/Nx;dy=(yf-y0)/Ny;
x1=x0:dx/2:xf;
y1=y0:dy/2:yf;
x2=dx/2:dx:xf;
y2=dy/2:dy:yf;
[X1,Y1]=meshgrid(y1,x1);
node1=[Y1(:) X1(:)];
[X2,Y2]=meshgrid(y2,x2);
node2=[Y2(:) X2(:)];
node1=num2str(node1);
node2=num2str(node2);
node=sortrows(setdiff(node1,node2,'rows'),2);
node=str2num(node);
node=sortrows(node,2);
node=sortrows(node,1);
node=sortrows(node,2);
