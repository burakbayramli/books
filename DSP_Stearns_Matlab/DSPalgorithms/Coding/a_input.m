function [bit,jb]=a_input(y,jb,ipr)
% Function used by a_decode to pull one bit from y.
% bit  =bit pulled: 1 or 0.
% jb   =bit location in y - incremented and output as jb+1.
% ipr has the same effect here as in a_decode when =2.
iy=fix(jb/8)+1;
sh=[1 2 4 8 16 32 64 128];              %sh =shift operator
% Byte=0 if length of y is exceeded.
if iy>length(y),
	byte=0;
else
   byte=double(y(iy));
end
ybit=7-rem(jb,8)+1;
bit=rem(fix(byte/sh(ybit)),2);
if ipr==2,
   fprintf('Pulled%2.0f at jbit=%7.0f\n',bit,jb);
end
jb=jb+1;
