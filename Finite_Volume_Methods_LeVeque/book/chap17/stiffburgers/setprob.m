fid = fopen('setprob.data');
iode = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
tau = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
beta = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
fclose(fid);

