function [handles] = res_ts(handles,irk)
ark = handles.Data.ark;
cfl = handles.Data.CFLNumber;
dt  = handles.Data.dt;
vol = handles.Data.vol;
rhs = handles.Data.rhs;
%switch handles.Data.meth
%    case 'ejam'
        fac = ark(irk)*cfl;
%    otherwise
%        fac = cfl;
%end
adtv= fac*dt./vol;
adtv= [adtv adtv adtv];
rhs = adtv.*rhs;

handles.Data.rhs = rhs;
