% read in parameters for acoustics

fid = fopen('setprob.data');
rho = fscanf(fid,'%g',1);   fscanf(fid,'%s',1);
bulk = fscanf(fid,'%g',1);   fscanf(fid,'%s',1);
beta = fscanf(fid,'%g',1);   fscanf(fid,'%s',1);
fclose(fid);

