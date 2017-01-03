function tsprint(y,cstruc,varargin)
% PURPOSE: print time-series matrix or vector with dates and column labels
%---------------------------------------------------
% USAGE:     tsprint(y,cstruc,begp,endp,vnames,fmt) 
%        or: tsprint(y,cstruct,vnames), prints entire series with names
%        or: tsprint(y,cstruct), entire series, no variable names
%        or: tsprint(y,cstruct,fmt) entire series, using fmt
%        or: tsprint(y,cstruct,vnames,fmt) entire series w/names and fmt
%        or: tsprint(y,cstruct,begp,endp) partial series no names or fmt
% where: y       = matrix (or vector) of series to be printed
%        cstruc  = a structure returned by cal()
%        begp    = the beginning observation to print
%        endp    = the ending period to print,
%        vnames  = a string matrix of names for a header (optional)
%                  e.g. vnames = strvcat('y','x1,'x2','x3');
%        fmt     = a format string, e.g., '%12.6f' or '%12d'
%---------------------------------------------------
% e.g.    cstr = cal(1980,1,12);
%         tsprint(y,cstr,13,24), would print data for 1981
%
%     or: tsprint(y,cstr,ical(1981,1,cstr),ical(1981,12,cstr)),
%         which would print the same data for 1981
%---------------------------------------------------
% SEE ALSO: tsplot, mprint
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if ~isstruct(cstruc)
   error('tsprint: requires a calendar structure');
end;
nargs = length(varargin);
[nobs nvar] = size(y); fmt = '%10.4f';
begp = 1; endp = nobs; nflag = 0;       % set defaults
fid_flag = 0;
nargs = length(varargin);               % find the # of input arguments
if nargs == 0                 % no user-supplied vnames or dates or fmt
% rely on defaults
elseif nargs == 1             % no dates but vnames or fmt
 [testf testg] = size(varargin{1});
 if testf == 1                % we have a format
  fmt = varargin{1};          % replace default fmt with user fmt
 else                         % we have vnames
  nflag = 1;                  % set flag for vnames
  vnames = varargin{1};       % pull-out user variable names
 end;
elseif nargs == 2;            % either begp,endp or names and fmt
 [testf testg] = size(varargin{1});
 if testf == 1                % we have a format or begp
    if isnumeric(varargin{1}) % we have begp, endp
     begp = varargin{1};      % pull-out begp
     endp = varargin{2};      % pull-out endp
    end;
 else                         % we have vnames, fmt
    vnames = varargin{1};     % pull-out vnames
    fmt = varargin{2};        % pull-out format
    nflag = 1;                % set flag for vnames
 end;
elseif nargs == 3             % begp,endp with either vnames or fmt
 [testf testg] = size(varargin{3});
 if testf == 1                % we have a format
  begp = varargin{1};
  endp = varargin{2};
  fmt = varargin{3};
 else                         % we have vnames
  nflag = 1;
  begp = varargin{1};
  endp = varargin{2};
  vnames = varargin{3}; 
 end;
elseif nargs == 4             % we have everything
 nflag = 1;
 begp = varargin{1};
 endp = varargin{2};
 vnames = varargin{3};
 fmt = varargin{4};
end; % end of input checking

if nflag == 0 % no variable names supplied, make some up
  vnames = [];
  for i=1:nvar
     snames = 'series';
     name = [snames num2str(i)];
     vnames = strvcat(vnames,name);
    end;
[visze nsize] = size(vnames);
else
  [vsize nsize] = size(vnames); % error check vnames argument
  if vsize ~= nvar; error('Wrong # vnames in tsprint'); end;
end;

rnames = 'Date';
for k=begp:endp;
rnames = strvcat(rnames,tsdate(cstruc,k));
end;

in.rnames = rnames;
in.fmt = fmt;
in.cnames = vnames;
mprint(y(begp:endp,:),in);


 



