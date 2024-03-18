function foil = foilfileread(filename,np)
% reads UIUC, Lednicer, Tornado and MSES blade.xxx files
% into standard UIUC ("Selig") array foil, TE -> upper -> LE -> lower -> TE
% copied in part from Mark Barton's code on UIUC
%====
% file must be ascii
% first line must be a comment like the name
% if blade-file 'blade.xxxx'
% else
%    file name should include three-letter extension like .txt or .dat
%        if not, .txt is assumed
if nargin < 2
    np = 50;
end
[pthstr,filnm,ext]=fileparts(filename)
blfil = strcmpi(filnm,'blade');
if isempty(ext)
    ext ='.txt';
else
    ext = '';
end
if strcmpi(filnm(1:4),'naca')
    'naca'
    if ~(length(filnm) == 8 || length(filnm)==9)
        error('wrong nacaxx...')
    else
        tmp = filnm(5:end);
        foil = naca45prof(str2num(tmp),np);
    end
    return
end
fid = fopen([filename ext],'r');
if (fid == -1)
    error(['error opening ',[filename ext]]);
end
dum = fgetl(fid); % skip first line
if blfil          % skip the xmin xmax ...line
    dum = fgetl(fid); 
end
pts   = [];
count = 0;
while 1 % read file line by line
    ff = fgetl(fid);
    if ff==-1 % eof
        break
    end
    [dum,cc]=sscanf(ff,'%f');
    if cc > 0 & cc < 4
        pts = [pts;dum];
        count = count+cc;
    end
end
%size(pts)
fclose(fid);
if ( (pts(1) == floor(pts(1))) && (pts(1)>5) )
    % "Lednicers" format, reshape to Selig
    %disp('lednicer')
    npts_top = pts(1);
    npts_bot = pts(2);
    top  = reshape(pts(3:2+2*npts_top),2,npts_top)';
    bot  = reshape(pts(3+2*npts_top:2+2*npts_top+2*npts_bot),2,npts_bot)';
    foil = [flipud(top);bot(2:end,:)];
    
elseif (length(pts)/2 == floor(length(pts)/2))
    %disp('selig')
    % "selig"  format assumed...
    npts = length(pts)/2;
    foil = reshape(pts,2,npts)';
    
else
    % n x y format assumed
    disp('n x y')
    npts = length(pts)/3;
    if (npts ~= floor(npts))
        fprintf(1,'error interpreting %s\n',filename);
        return;
    end
    foil = zeros(3,npts);
    foil(:) = pts;
    foil = foil(:,2:3);
end