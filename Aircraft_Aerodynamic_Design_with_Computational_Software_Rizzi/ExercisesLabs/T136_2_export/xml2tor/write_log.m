function dum = write_log(fid,geo)
global TraceOn
if TraceOn
    disp('xmlTorn/write_log');
end
% write log to logfile on file id fid
dum =[];
fprintf(fid,'Wings \n');
fprintf(fid,'-----\n');
fprintf(fid,'no.  name  type  #panels \n');
fprintf(fid,'--------------------------------------\n');
n = length(geo.winglist);
for k = 1:n
    tmp    = geo.winglist{k};
    wingno = tmp.seqno;
    fprintf(fid,'%2.0f %8s %8s %2.0f  \n',wingno,tmp.name,tmp.type,geo.nelem(wingno));
end
fprintf(fid,'\n\n');

fprintf(fid,'Panels \n');
fprintf(fid,'------\n');
fprintf(fid,'wingno  panelno. ny      nx      fnx_LE  fnx_TE \n');
fprintf(fid,'------------------------------------------------\n');
[n,m] = size(geo.fc);
for k = 1:n
    for j = 1:geo.nelem(k)
        if j == 1
    fprintf(fid,' %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f \n',...
                 k,j,geo.ny(k,j),geo.nx(k,j),geo.fnx_LE(k,j),geo.fnx(k,j));
        else
            fprintf(fid,'    %5.0f %5.0f %5.0f %5.0f %5.0f \n',...
                 j,geo.ny(k,j),geo.nx(k,j),geo.fnx_LE(k,j),geo.fnx(k,j));
        end
    end
end
fprintf(fid,'\n\n');

fprintf(fid,'Control surfaces \n');
fprintf(fid,'---------------- \n');
list = geo.cslist;
n = length(list);
fprintf(fid,' name   wing#  type deflect(o) part0--part1 cfrac dihed(o) sweep(o) \n' );
fprintf(fid,'-------------------------------------------------------------------------\n');
for k = 1:n
    dat = list{k};
    txt = '---';
    if ~isempty(dat.hingedata)
        txt = ['cfrac ',num2str(dat.hingedata(1)),' dihed ',num2str(dat.hingedata(2)),...
            ' sweep ', num2str(dat.hingedata(3))];
    end
    fprintf(fid,'%7s %2.0f      %1.0f   %5.0f     %2.0f--%2.0f %s \n',...
        dat.name,dat.wingno,dat.type,180/pi*dat.defl,dat.part0,dat.part1,txt);
end
