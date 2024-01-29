
load nodes.dat;

n=size(nodes,1);

fid=fopen('nodalTemp.dat','w+');

for i=1:n
  x=nodes(i,2);
  y=nodes(i,3);
  z=nodes(i,4);
  if z==0.0
     fprintf(fid,'%d  %.6f \n',i, 100.0);
  end
  if z==60.0
    fprintf(fid,'%d  %.6f \n',i, 40-x/2.0);
  end
end
fclose(fid);