function makeHTMLindex(htmlFile,verbose)
% makeHTMLindex  Creates HTML file listing contents of the NMM toolbox
%
% Synopsis:  makeHTMLindex
%            makeHTMLindex(htmlFile)
%            makeHTMLindex(htmlFile,verbose)
%
% Input:   htmlFile = (optional, string) name of HTML file to create.
%                     Default:  NMMfiles.html
%          verbose  = (optional) flag to turn off/on printing of directory
%                     contents.  Default:  verbose=1, print directory contents
%
% Output:  HTML file named 'htmlFile'.html.
%
%          Print count of m-files, data files and other files in
%          the local installation of the NMM toolbox

% Gerald Recktenwald, gerry@me.pdx.edu
% last revision: 19 Aug 2001

if nargin<1 | isempty(htmlFile),  htmlFile = 'NMMfiles.html';   end
if nargin<2,  verbose=1;   end

if strcmp(computer,'PCWIN')  % skip . and .. in directory listings; Windows hack
  dstart = 3;
else
  dstart = 1;
end

fhtml = fopen(htmlFile,'w');
beginHTML(fhtml);

thePath = getRootPath;                    %  Full path to root directory for m-files
dirNames = getDirList(thePath);           %  Matrix containing names of directories
tableOfContents(fhtml,dirNames,dstart);   %  Write table of contents structure to HTML file

% ---- Begin processing directories
ndirs = 0;  nmfiles = 0;  ndat = 0;  nc = 0;  nmex = 0;  npcodes = 0;
nufo = 0;   ufo = '';


for k=dstart:size(dirNames,1)

  dName = deblank(dirNames(k,:));      %  remove trailing blanks from directory name string
  if verbose
    fprintf('\n\nContents of %s directory in the NMM toolbox\n',dName)
  end
  dd = dir([thePath,dName]);           %  struct containing contents of current directory
  if ~verbose,  fprintf('.');  end     %  indicate progress
  beginDirectory(fhtml,dName);         %  boilerplate HTML for beginning of file list for this directory
  [md,nd] = size(dd);

  for j=dstart:md                      %  loop over files in the directory

    fName = dd(j).name;                %  next file name
    if verbose,  fprintf('\t%s\n',fName);  end

    switch exist(fName)                %  exist() returns indicator of file type
      case 7,
        ndirs = ndirs + 1;             %  item is another directory
      case 2,                          %  item is m-file or unknown type
        nlen = length(fName);
        if strcmp(fName(nlen-1:nlen),'.m')  %  does it end with '.m'?
          if length(fName)<8 | ~strcmp(lower(fName(1:8)),'contents')  %  Don't create link for contents.m
             nmfiles = nmfiles + 1;
             fprintf(fhtml,mfileNameHeadTag(fName(1:nlen-2),dName));  %  Link for current file name
          end
        elseif strcmp(fName(nlen-3:nlen),'.dat')  %  or '.dat'?
          if length(fName)<8 | ~strcmp(lower(fName(1:8)),'contents')  %  Don't create link for contents.m
             ndat = ndat + 1;
             fprintf(fhtml,mfileNameTag(fName(1:nlen),dName));        %  Link for *full* file name
          end
        elseif strcmp(fName(nlen-1:nlen),'.c')    %  or '.c?
          nc = nc + 1;
        else                           %  else it's an unidentified file object
          nufo = nufo + 1;
          ufo = str2mat(ufo,fName);
        end
      case 3,                          %  item is a mex file
        nmex = nmex + 1;
      case 6,                          %  item is a pcode file
        npcodes = npcodes + 1;
    end
  end
  endDirectory(fhtml);    %  boilerplate HTML for end of file list for this directory
end

endHTML(fhtml);
fclose(fhtml);

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

fprintf('\n\n\tHTML file created:  %s\n',htmlFile);
fprintf('\tin directory:  %s\n',pwd);

% =======================
function beginDirectory(fout,dirName)
% beginDirectory  Heading and other boilerplate for beginning of a directory listing.
%                 The heading contains a named link that is refered to by the table
%                 of contents.
%
%  see also:  tableOfContents()

fprintf(fout,'<br>\n');
fprintf(fout,'<hr width="90%%" align=left>\n');
fprintf(fout,headingTag(2,sprintf('%s',anchor(sprintf('Contents of %s directory',dirName),sprintf('name="%s"',dirName)))));
% fprintf(fout,'<br>\n');
fprintf(fout,'<pre>\n');


% =======================
function h = endDirectory(fout)
% endDirectory  Boilerplate for end of a directory listing

fprintf(fout,'</pre>\n');
fprintf(fout,'<p align=center>\n');
fprintf(fout,'<A HREF="#contents"><SMALL>Back to List of Directories</SMALL></A>\n');
fprintf(fout,'</p>\n');
fprintf(fout,'\n\n');

% =======================
function beginHTML(fout)
% beginHTML  Boilerplate for beginning of the HTML file

fprintf(fout,'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"\n');
fprintf(fout,'                      "http://www.w3.org/TR/REC-html40/loose.dtd">\n');
fprintf(fout,'<HTML>\n');
fprintf(fout,'<HEAD>\n\n');
fprintf(fout,'<TITLE>m-files by Chapter</TITLE>\n\n');
fprintf(fout,'</HEAD>\n');

fprintf(fout,'<table summary="Top border setup">\n');
fprintf(fout,'<tr align="left">\n');
fprintf(fout,'	<td>\n');
fprintf(fout,'		<img src="prenhallBookCover.jpg" alt="Numerical Methods with MATLAB: Implementations and Applications">\n');
fprintf(fout,'	</td>\n');
fprintf(fout,'	<td width="50">&nbsp;</td>\n');
fprintf(fout,'	<td align="right">\n');
fprintf(fout,'		<font face="arial" size="+1">\n');
fprintf(fout,'			Numerical Methods<br>with MATLAB:<br>\n');
fprintf(fout,'		</font>\n');
fprintf(fout,'		<br>\n');
fprintf(fout,'		<font face="arial">\n');
fprintf(fout,'			Implementations<br>and Applications<br><br>\n');
fprintf(fout,'			Gerald Recktenwald\n');
fprintf(fout,'		<BR><BR>&copy 2000, Prentice Hall<br>\n');
fprintf(fout,'		ISBN: 0201308606\n');
fprintf(fout,'		</font>\n');
fprintf(fout,'	</td>\n');
fprintf(fout,'</tr>\n');
fprintf(fout,'</table>\n');

hrule(fout); breakLine(fout);
parBegin(fout);
fprintf(fout,'This page contains links to individual m-files and data files appearing in\n');
fprintf(fout,'<em>Numerical Methods with MATLAB: Implementations and Applications</em>\n');
fprintf(fout,'by G.W. Recktenwald,  copyright 2000, Prentice Hall.');
parEnd(fout);
[v,vn,vd] = nmmVersion;
parBegin(fout);
fprintf(fout,'The files listed here are for NMM Toolbox version %5.2f, %s.\n',vn,vd);
fprintf(fout,'The latest version of the NMM Toolbox can be downloaded from ');
fprintf(fout,'<a href = "http://www.me.pdx.edu/~gerry/nmm">www.me.pdx.edu/~gerry/nmm</a>.\n');
parEnd(fout);
parBegin(fout);
fprintf(fout,'<font size="-1">This page automatically generated by makeHTMLindex on %s</font>\n',datestr(now));
parEnd(fout);

% =======================
function breakLine(fp)
fprintf(fp,'\n<br>\n');

% =======================
function hrule(fp)
fprintf(fp,'\n<hr>\n');

% =======================
function parBegin(fp)
fprintf(fp,'\n<p>\n');

% =======================
function parEnd(fp)
fprintf(fp,'\n</p>\n');

% =======================
function endHTML(fout)
% endHTML  Boilerplate for end of the HTML file

fprintf(fout,'</BODY>\n');
fprintf(fout,'</HTML>\n');

% =======================
function tableOfContents(fout,dirNames,dstart)
% tableOfContents  Build a list of named directory links.  These links point to respective
%                  lists of directory contents in the same HTML file.
%
%  see also:  beginDirectory()

fprintf(fout,'<hr>\n');
fprintf(fout,headingTag(2,sprintf('%s',anchor('Directories in the NMM Toolbox','name="contents"'))));
fprintf(fout,'\n<ul type=disc>\n');
for k=dstart:size(dirNames,1)
  dName = deblank(dirNames(k,:));
  fprintf(fout,'\t<li> %s\n',anchor(dName,sprintf('href="#%s"',dName)));
end
fprintf(fout,'</ul>\n\n');


% =======================
function h = headingTag(level,headText)
% headingTag  Create a named heading tag corresponding to a directory

lev = fix(level);
if lev>6 | lev<1,  error(sprintf('Heading level %d not allowed in HTML specification',lev));  end

h = sprintf('<h%d>%s</h%d>\n',lev,headText,lev);

% =======================
function tag = mfileNameTag(mfile,itsPath)
% mfileNameTag  Create a one-line link for an m-file.  Link only contains name of mfile
%               This is useful for files (e.g. data files) that do not the H1 line of
%               a typical m-file.
%
% Input:   mfile   = name of the mfile without .m extension
%          itsPath = (string matrix) path to the mfile in the nmm toolbox.  Each directory level
%                    is stored on a separate row.
%                    Examples:  itsPath = 'nmm';  itsPath = ['nmm';'data']
%
% Output:  tag = (string) containing html code for a link to an mfile.  The highlighted text
%                is the name of the mfile.  Auxillary text is the H1 line from the function

pathString = [itsPath(1,:),'/'];   %  intialize pathString
[m,n] = size(itsPath);
for k=2:m  % loop only if itsPath contains multiple directory levels
  pathString = [pathString(k,:),'/'];  %  append next level
end
anchorSpec = sprintf('href="%s%s"',pathString,mfile);   %  pathString contains terminating '/'
tag = sprintf('  %s\n',anchor(mfile,anchorSpec));

% =======================
function tag = mfileNameHeadTag(mfile,itsPath)
% mfileNameHeadTag  Create a one-line link for an m-file.  Link contains name of mfile and
%                   the H1 line from the function listing
%
% Input:   mfile   = name of the mfile without .m extension
%          itsPath = (string matrix) path to the mfile in the nmm toolbox.  Each directory level
%                    is stored on a separate row.
%                    Examples:  itsPath = 'nmm';  itsPath = ['nmm';'data']
%
% Output:  tag = (string) containing html code for a link to an mfile.  The highlighted text
%                is the name of the mfile.  Auxillary text is the H1 line from the function

fid = fopen(sprintf('%s.m',mfile),'rt');
if fid<0, error(sprintf('Could not open source for %s.m'));  end

str = fgetl(fid);          %  read first line of the file
                           %  First line is the function definition line for function m-files
                           %  or is assumed to be an H1-like comment statment for scripts
if strcmp(lower(str(1:4)),'func'),
   str = fgetl(fid);       %  read second line as H1 line if mfile is a function
end
fclose(fid);
for k=1:2
  [next,str] = strtok(str);  %  go to next non-whitespace token (?? some spaces are left)
end
while isspace(str(1)),  str(1) = [];  end   %  strip off any leading blanks
pathString = [itsPath(1,:),'/'];            %  intialize pathString
[m,n] = size(itsPath);
for k=2:m                                   %  loop only if itsPath has multiple directory levels
  pathString = [pathString(k,:),'/'];       %  append name of next directory level to pathstring
end
anchorSpec = sprintf('href="%s%s.m"',pathString,mfile);   %  pathString contains terminating '/'

maxWidth = 16;                              %  number of characters alloted to widest m-file name
nblanks = maxWidth - length(mfile);
if nblanks>2,  padding = blanks(nblanks);  else,  padding = '  ';  end
tag = sprintf('  %s %s\n',anchor(mfile,anchorSpec),[padding,str]);

% =======================
function a = anchor(anchorText,anchorSpec)
% anchorText  Build an anchor string of the form  <a anchorSpec> anchorText </a>
%
% Input:  anchorText = text to be highlighted in link
%         anchorSpec = (optional) specification of anchor type,
%                      Example:  anchorSpec = 'href="dir/subdir/mfile.m"'
%
% Output: a = string containing html link (anchor)

if nargin==1
  a = sprintf('<a>%s</a>',anchorText);
elseif nargin==2
  a = sprintf('<a %s>%s</a>',anchorSpec,anchorText);
else
  error(sprintf('%d arguments supplied to anchor',nargin));
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
