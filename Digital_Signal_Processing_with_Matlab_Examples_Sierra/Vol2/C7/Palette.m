% Show the palette of colors

function [pal]=Palette(C)
%small square
sqR=zeros(32,32); sqG=zeros(32,32);sqB=zeros(32,32);
%large square
WW=4*32;
pltR=zeros(WW,WW); pltG=zeros(WW,WW);pltB=zeros(WW,WW);
pal=zeros(WW,WW,3);

nc=1; nl=1; % line and column counters
% square filling
for nsq=1:16,   
  for i=1:32,
     for j=1:32,
      sqR(i,j)=C(1,nsq); sqG(i,j)=C(2,nsq); sqB(i,j)=C(3,nsq);
     end;
  end;
  % pointers:
  bl=1+((nl-1)*32); el=bl+31; il=bl:el;
  bc=1+((nc-1)*32); ec=bc+31; ic=bc:ec;
  % putting small square in large square
  pltR(il,ic)=sqR; pltG(il,ic)=sqG; pltB(il,ic)=sqB; 
  % counters update
  if nc==4,
     nc=1; nl=nl+1;
  else
     nc=nc+1;
  end;   
end;

pal(:,:,1)=pltR; pal(:,:,2)=pltG; pal(:,:,3)=pltB;

