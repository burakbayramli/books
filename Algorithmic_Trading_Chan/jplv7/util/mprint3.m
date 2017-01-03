function out=mprint3(b,se,t,info)
% PURPOSE: Pretty-prints a set of matrices together by stacking the 
% (i,j) elements of each.  Allows for separate formatting of 
% each.  Designed to printing parm. est, se and t-stat.
%---------------------------------------------------------------
% USAGE: out = mprint3(b,se,t,info)
%
%  b         first matrix
%  se        second matrix
%  t         third matrix (optional)
%  info      structure variable (all optional)  [default]
%   .bfmt    format string for b matrix              ['%8.4f']
%   .sefmt   format string for se matrix             ['(%8.4f)']
%   .tfmt    format string for t matrix              ['%8.2f']
%   .vspc    number of blank lines between blocks    [1]
%   .hspc    spacing between colums                  [3]
%   .ldum    set to 1 for LaTeX version              [0]
%
%---------------------------------------------------------------
% NOTE: No file output option is given.  To save to a file use
%   diary('filename'); disp(out); diary;
%---------------------------------------------------------------
% SEE ALSO: mprint3_d, mprint
%---------------------------------------------------------------


% Written by  Mike Cliff,  UNC Finance   mcliff@unc.edu
% CREATED  12/15/98

%======================================================================
%  INITIALIZATIONS
%======================================================================

if nargin < 4,  info.null = 1; end
if nargin == 2
  dum2 = 1;
else 
  if isstruct(t), dum2 = 1;
  else, dum2 = 0; end
end
  
[R,C] = size(b);
if ~isfield(info,'vspc'), info.vspc = 1; end
if ~isfield(info,'hspc'), info.hspc = 3; end
if ~isfield(info,'ldum'), info.ldum = 0; end

if info.ldum == 1
  amp = repmat(' & ',R*(3 - dum2 + info.vspc)-info.vspc,1);
  eol = repmat(' \\',R*(3 - dum2 + info.vspc)-info.vspc,1);
else
  amp = repmat(' ',R*(3 - dum2 + info.vspc)-info.vspc,info.hspc);
  eol = repmat(' ',R*(3 - dum2 + info.vspc)-info.vspc,1);  
end

if ~isfield(info,'bfmt'), info.bfmt = '%7.4f'; end
if ~isfield(info,'sefmt'), info.sefmt ='(%7.4f)'; end
if ~isfield(info,'tfmt'), info.tfmt = '%7.2f'; 
end

if rows(info.bfmt) == 1, info.bfmt = repmat(info.bfmt,C,1); end
if rows(info.sefmt) == 1, info.sefmt = repmat(info.sefmt,C,1); end
if rows(info.tfmt) == 1, info.tfmt = repmat(info.tfmt,C,1); end

  out = repmat(' ',R*(3 - dum2 + info.vspc)-info.vspc,1);


%======================================================================
%  BUILD UP OUTPUT
%======================================================================

for c = 1:C  
  temp = []; 
  for r = 1:R
    b0 = num2str(b(r,c),info.bfmt(c,:));
    se0 = num2str(se(r,c),info.sefmt(c,:));
% ---- Do this if we have 3 matrices ----------------------------------
    if dum2 == 1
      temp = strvcat(temp,b0,se0);
      for i = 1:info.vspc
        temp = strvcat(temp,blanks(1));
      end
% ---- Otherwise we have 2 matrices ----------------------------------
    else
      t0 = num2str(t(r,c),info.tfmt(c,:));
      temp = strvcat(temp,b0,se0,t0);
% ---- Add blank lines as specified ----------------------------------
      for i = 1:info.vspc
        temp = strvcat(temp,blanks(1));
      end
    end
    temp = strjust(temp,'right');      % A formatting nicety
  end
  temp = temp(1:rows(out),:);          % Fix to remove trailing blank rows
  out = [out amp temp];                % Slap new column on existing
end

out = [out eol];                       % Add eol characters
