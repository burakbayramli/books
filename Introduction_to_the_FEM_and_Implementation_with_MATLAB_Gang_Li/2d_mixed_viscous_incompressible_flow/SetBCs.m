function SetBCs(nodes)
fid=fopen('bcsvp.dat','w+');  % open file to write to
for i=1:size(nodes,1)
  x=nodes(i,2);
  y=nodes(i,3);
  if y==1.0      % top edge: velocity boundary condition
    fprintf(fid,'%d  %d  %.10f\n',i, 1, 1);
    fprintf(fid,'%d  %d  %.10f\n',i, 2, 0);
    if x==0.5   % top edge center: zero pressure
      fprintf(fid,'%d  %d  %.10f\n',i, 3, 0);
    end
  elseif x==0.0 || y==0.0 || x==1.0  % the other 3 edges
    fprintf(fid,'%d  %d  %.10f\n',i, 1, 0); % x-velocity =0
    fprintf(fid,'%d  %d  %.10f\n',i, 2, 0); % y-velocity =0 
  end
end
fclose(fid);