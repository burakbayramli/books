function nmmCheck(verbose)
% nmmCheck  Verify installation of NMM toolbox
%
% Synopsis:  nmmCheck
%            nmmCheck(verbose)
%
% Input:   verbose = (optional) flag to turn off/on printing of directory
%                    contents.  Default:  verbose=1, print directory contents
%
% Output:  Print count of m-files, data files and other files in
%          the local installation of the NMM toolbox

if nargin<1,  verbose=1;  end
debug = 0;

thePath = getRootPath;             %  Full path to root directory for m-files
dirNames = getDirList(thePath);    %  Matrix containing names of directories in NMM toolbox

% ---- Begin processing directories
ndirs = 0;  nmfiles = 0;  ndat = 0;  nc = 0;  nmex = 0;  npcodes = 0;
nufo = 0;   ufo = '';

if strcmp(computer,'PCWIN')  % skip . and .. in directory listings; Windows hack
  dstart = 3;
else
  dstart = 1;
end

for k=dstart:size(dirNames,1)

  dName = deblank(dirNames(k,:));      %  remove trailing blanks from directory name string
  if verbose
    fprintf('\n\nContents of %s directory in the NMM toolbox\n',dName)
  end
  dd = dir([thePath,dName]);           %  struct containing contents of current directory
  if ~verbose,  fprintf('.');  end     %  indicate progress
  [md,nd] = size(dd);

  for j=dstart:md                      %  loop over files in the directory

    fName = dd(j).name;                %  next file name
    if verbose,  fprintf('\t%s\n',fName);  end

    switch exist(fName)                %  exist() returns indicator of file type
      case 7,
        ndirs = ndirs + 1;             %  item is another directory
      case 2,                          %  item is m-file or unknown type
        nlen = length(fName);
        if strcmp(fName(nlen-1:nlen),'.m')        %  does it end with '.m'?
          if length(fName)<8 | ~strcmp(lower(fName(1:8)),'contents')  %  Don't create link for contents.m
            nmfiles = nmfiles + 1;
          end
        elseif strcmp(fName(nlen-3:nlen),'.dat')  %  or '.dat'?
          if length(fName)<8 | ~strcmp(lower(fName(1:8)),'contents')  %  Don't create link for contents.m
             ndat = ndat + 1;
          end
        elseif strcmp(fName(nlen-1:nlen),'.c')    %  or '.c?
          nc = nc + 1;
        else                     %  else it's an unidentified file object
          nufo = nufo + 1;
          ufo = str2mat(ufo,fName);
        end
      case 3,                    %  item is a mex file
        nmex = nmex + 1;
      case 6,                    %  item is a pcode file
        npcodes = npcodes + 1;
    end
  end
end

% --- Print summary of NMM toolbox contents
fprintf('\n\nNMM Toolbox Contents Summary\n');
fprintf('\t%4d  m-files\n',nmfiles);
fprintf('\t%4d  data files\n',ndat);
if nmex>0,    fprintf('\t%4d  mex files\n',nmex);               end
if nc>0,      fprintf('\t%4d  C source files\n',nc);            end
if npcodes>0, fprintf('\t%4d  pcode files\n',npcodes);          end
if ndirs>0,   fprintf('\t%4d  secondary directories\n',ndirs);  end
if nufo>0,    fprintf('\t%4d  unidentified files\n',nufo);      end

fprintf('    ------------------------\n')
fprintf('\t%4d  total files\n',nmfiles+ndat+nmex+nc+npcodes+ndirs+nufo);

% --- List UFOs
if nufo>0
  fprintf('\n\nUnidentified files in NMM toolbox:\n');
  [m,n] = size(ufo);
  for k=2:m
    fprintf('\t%s\n',ufo(k,:));
  end
end

% ===================
function dirNames = getDirList(thePath)
% getDirList  Build list of directories in the toolbox
%
% Input:  thePath = path to a directory containing mfiles
%
% Output: dirNames = string matrix containing file names, one per row

d = dir(thePath);   %  returns a struct with four fields
[m,n] = size(d);
dirNames = '';      %  initialize string matrix for director names
for k=1:m
  if exist(d(k).name)==7   %  true if d(k).name is a directory
    dirNames = str2mat(dirNames,d(k).name);
  end
end
if size(dirNames,1)>1
 dirNames(1,:) = [];  %  delete first (empty) row created when labels matrix initialized
else
 fprintf('dirNames = \n');
 disp(dirNames);
 error('No directories found in NMM toolbox!?');
end

% =================================
function thePath = getRootPath
% getRootPath  Return a string containing the root directory for the toolbox

p = which('nmmCheck');    %  string containing path to NMM toolbox
                          %  Assumes 'nmmCheck.m' is already in MATLAB path!!!
if isempty(p)
  error('NMM toolbox is not found! Has nmmCheck.m been moved?');
else
  ns = findstr(p,'nmm');
  thePath = p(1:ns(1)+3);   %  path to the nmm toolbox
  fprintf('\nThe path to the NMM toolbox is\n\n\t%s\n\n',thePath)
end
