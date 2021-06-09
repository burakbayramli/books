function [v,vn,vd] = nmmVersion
% nmmVersion  Returns version number of NMM toolbox currently installed
vnum = 1.03;
vdate = '20-Aug-2001';
v = sprintf('NMM Toolbox Version %5.3f, %s, copyright Gerald W. Recktenwald',vnum,vdate);
if nargout==3
  vn = vnum;  vd = datestr(datenum(vdate));
elseif nargout==2
  vn = vnum
end
