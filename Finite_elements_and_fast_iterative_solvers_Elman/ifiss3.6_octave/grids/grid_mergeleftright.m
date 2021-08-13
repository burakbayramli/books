function [xy,mv,left,bottom,top,right,mbound,x,y] = ...
   grid_mergeleftright(xyl,mvl,leftl,rightl,bottoml,topl,mboundl,xl,yl,...
   xyr,mvr,leftr,rightr,bottomr,topr,mboundr,xr,yr);
%GRID_MERGELEFTRIGHT  merger of inlet/outlet channel domain 
% 
%   IFISS function: DJS; 23 November 2014.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('   Merger of two x-channel grids \n')
nleft=length(rightl); nright=length(leftr);
% right domain numbering is modified
% identify the end nodes on the left
startl=rightl(1); endl=rightl(end);
if endl~=startl+nleft-1, 
    error('left domain is not y-numbered, try again.'), end
% identify the end nodes on the right
startr=leftr(1); endr=leftr(end);
if endr~=startr+nright-1, 
    error('right domain is not y-numbered, try again.'), end
xloop=leftr(2)-leftr(1); %fprintf('right increment is %g\n', xloop)
% find y-values of zipped nodes on left
yzip=xyl(rightl,2);
% find bottom of zip on the right
kb=find(xyr(leftr,2)==yzip(1));
% find top of zip on the right
kt=find(xyr(leftr,2)==yzip(end));
refleftr=leftr(kb:kt);
% find y-values of zipped nodes on right
yzipz=xyr(refleftr,2);
fprintf('   zip distance is %9.4e ... it should be close to zero!\n',...
          norm(yzip-yzipz,inf))
%
% merge xy data
nvtxl=size(xyl,1); nell=length(mvl(:,1));
xyl(rightl,:)=[];
xy=[xyl;xyr];
x=unique([xl,xr]); y=unique([yl,yr]);
% merge mv data
extra=nvtxl-nleft;
mvr=mvr+extra; refleftr=refleftr+extra;
kk=find(mboundl(:,2)==2); nkk=length(kk);
list=mboundl(kk,1); mboundl(kk,:)=[];
      mvl(list,2) = refleftr(1:2:end-1);  
      mvl(list,3) = refleftr(3:2:end);   
      mvl(list,6) = refleftr(2:2:end-1); mvl(list,:);     
mv=[mvl;mvr];
%
% remove all the right domain reference edge macros
mboundr(:,1)=mboundr(:,1)+nell; 
firstm=(refleftr(1)+1-extra)/2;
lastm=(refleftr(end)-(extra+1))/2;
mboundr(firstm:lastm,:)=[];
kkk=find(mboundr(:,2)==2);  mboundr(kkk,:)=[]; 
%
% reset boundary data
left=leftl; right=rightr+extra;
top=[topl(1:end-1);leftr(kt:end)+extra;topr+extra];
bottom=[bottoml(1:end-1);leftr(1:kb)+extra;bottomr+extra];
% reset boundary edge data
mbound=[mboundl;mboundr];
return
