%function[a,b]=wavei(z,x,y,mmin,mmax,sub1,sub2,cbar_label)
%
% function to make arbitrary image type color plots
%
% Kevin D. LePage
% SACLANTCEN
% 2/2/98
%
% INPUTS
%
% z	vertical (color) information (matrix), must be real
% x	optional x axis scale
% y	optional y axis scale
% mmin	minimum value of z to plot (optional)
% mmax	maximum value of z to plot (optional)
% sub1	first subplot command (optional, default 1)
% sub2 	second subplot command (optional, default 1)
% cbar_label
%	colorbar label (defualt dB)
%
% OUTPUTS
%
% a	handle to color figure
% b	handle to colorbar

function[a,b]=wavei(z,x,y,mmin,mmax,sub1,sub2,cbar_label)

% check to see that the argument is real
pcolor_flag=0;

if isreal(z)~=1

fprintf('input argument is not real, breaking\n')

return

end

% replace infinite values with lowest non-infinite value

if sum(find(isinf(z)))~=0

fprintf('\rWarning: %d infinities were found, replacing with smallest real value',sum(find(isinf(z))))

if nargin<5

z(find(isinf(z)))=min(min(z(find(isinf(z)==0))))*ones(size(find(isinf(z))));

else

z(find(isinf(z)))=mmin*ones(size(find(isinf(z))));

end

end

%if sum(find(isnan(z)))~=0

%fprintf('\rWarning: %d NaN''s were found, replacing with smallest real value',sum(find(isnan(z))))

%if nargin<5

%z(find(isnan(z)))=min(min(z(find(isnan(z)==0))))*ones(size(find(isnan(z))));

%else

%z(find(isnan(z)))=mm*ones(size(find(isnan(z))));

%end

%end

% check to see if arguments are consistent with pcolor

if nargin<3

x=[1 size(z,2)];

y=[1 size(z,1)];

else

if ((length(x)==size(z,2)) & (length(y)==size(z,1)))

x=ones(size(z,1),1)*x(:)';

y=y(:)*ones(1,size(z,2));

end


if ((size(x)==size(z))&(size(y)==size(z)))

pcolor_flag=1;

end

end

if nargin>4

if isempty(find(z<mmin))

z(size(z,1),size(z,2))=mmin;

else

z(find(z<mmin))=mmin*ones(size(find(z<mmin)));

end

if isempty(find(z>mmax))

z(size(z,1)-1,size(z,2))=mmax;

else

z(find(z>mmax))=mmax*ones(size(find(z>mmax)));

end

end

if nargin>6

if((sub2==1)&(sub1>1))

clg

end

if sub1>6

subplot(ceil(sub1/3),3,sub2)

elseif sub1>3

subplot(ceil(sub1/2),2,sub2)

elseif sub1>1

subplot(sub1,1,sub2)

else

end

end

if pcolor_flag

a=pcolor(x,y,z);shading('flat');colormap('jet');

a=gca;

if exist('sub1')

if sub1>6

%b=colorbah('vert',6);

b=colorbar;

else

b=colorbar;

end

else

b=colorbar;

end

else

imagesc(x,y,z);colormap('jet');

axis('xy');

a=gca;

if exist('sub1')

if sub1>6

%b=colorbah('vert',6);

b=colorbar;

else

b=colorbar;

end

else

b=colorbar;

end

end

axes(b);

if exist('sub1')

if sub1>6

set(b,'FontSize',6)

end

end

if nargin==8

ylabel(cbar_label)

else

ylabel('dB')

end

axes(a)

if exist('sub1')

if sub1>6

set(a,'FontSize',6)

end

end
