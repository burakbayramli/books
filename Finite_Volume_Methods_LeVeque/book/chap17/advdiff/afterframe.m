

hold on
axis([-1 2 -0.5 2.5])

% exact solution:  
fid = fopen('setprob.data');
u = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
dcoef = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
fclose(fid);

if (t>0)
  xx = -1:.01:4;
  v = erfc((xx-u*t)/sqrt(4*dcoef*t));
  vgrid = erfc((x-u*t)/sqrt(4*dcoef*t))';
  err1 = sum(abs(q-vgrid)) * dx;
  disp(['1-norm of error = ' num2str(err1)])
else
  v = zeros(1,length(xx));
  v(find(xx<0)) = 2;
  end

plot(xx,v)


title(['time t = ' num2str(t)])
hold off

