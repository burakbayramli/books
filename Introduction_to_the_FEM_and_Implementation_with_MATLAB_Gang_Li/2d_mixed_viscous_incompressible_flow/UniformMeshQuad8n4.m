function [nodes, elements]=UniformMeshQuad8n4(sx,sy,ex,ey,nx,ny)

[x,y]=meshgrid(sx:(ex-sx)/nx/2:ex, sy:(ey-sy)/ny/2:ey);
nodes=zeros(3*nx*ny + 2*nx + 2*ny +1,3);
elements=zeros(nx*ny,9);
pelements=zeros(nx*ny,9);
nids=zeros(2*ny+1,2*nx+1);

%create nodes
fid=fopen('nodes.dat','w+');
fid2=fopen('pnodes.dat','w+');
k=1;
p=1;
for i=1:2*ny+1   %columns
  for j=1:2*nx+1
    if mod(i,2)==0 && mod(j,2)==0
      continue;
    end
    nids(i,j)=k;
    nodes(k,1)=k;
    nodes(k,2)=x(i,j);
    nodes(k,3)=y(i,j);
    fprintf(fid,'%d  %.10f  %.10f\n',k, x(i,j), y(i,j));
    if mod(i,2)~=0 && mod(j,2)~=0
      fprintf(fid2,'%d  %d\n',k, p);
      p=p+1;
    else
      fprintf(fid2,'%d  %d\n',k, 0);
    end
    k=k+1;
  end
end
fclose(fid);
fclose(fid2);

%create elements
fid=fopen('elements.dat','w+');
k=1;
for i=1:ny    %rows
  for j=1:nx  %columns
    elements(k,1)=k;
    elements(k,2)=nids(2*i-1,2*j-1);
    elements(k,3)=nids(2*i-1,2*j+1);
    elements(k,4)=nids(2*i+1,2*j+1);
    elements(k,5)=nids(2*i+1,2*j-1);
    elements(k,6)=nids(2*i-1,2*j);
    elements(k,7)=nids(2*i,2*j+1);
    elements(k,8)=nids(2*i+1,2*j);
    elements(k,9)=nids(2*i,2*j-1);   
    fprintf(fid,'%d %d %d %d %d %d %d %d %d \n',...
	    k,elements(k,2:9));
    k=k+1;
  end
end
fclose(fid);

%create elements
fid=fopen('pelements.dat','w+');
k=1;
for i=1:ny    %rows
  for j=1:nx  %columns
    elements(k,1)=k;
    elements(k,2)=nids(2*i-1,2*j-1);
    elements(k,3)=nids(2*i-1,2*j+1);
    elements(k,4)=nids(2*i+1,2*j+1);
    elements(k,5)=nids(2*i+1,2*j-1);
    fprintf(fid,'%d %d %d %d %d  \n',...
      k,elements(k,2:5));
    k=k+1;
  end
end
fclose(fid);

clear x; clear y; clear nids;