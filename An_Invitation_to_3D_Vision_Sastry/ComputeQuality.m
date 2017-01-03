%function Q = ComputeQuality(Ipi,xtt,goodfeat,wintx,winty)
%xtt: 2xN
%goodfeat: Nx1
%
%Purpose : finding the quality vector Qtt of xtt in Ipi
%
%support routine for 'trackdemo.m' (help trackdemo)
%
%
%Contributors to this code include: Jean-Yves Bouguet, Hailin Jin.
%Last updated 5/5/2003.
%
%DISTRIBUTED FREE FOR NON-COMMERCIAL USE
%Copyright (c) MASKS, 2003


function Qtt = ComputeQuality(Ipi,xtt,goodfeat,wintx,winty)

if size(xtt,2)~=length(goodfeat),
  disp('Invalid input in ComputeQuality');
  return;
end;

Qtt = zeros(length(goodfeat),1);
Index = find(goodfeat);
N = length(Index);
Q2 = zeros(N,1);
xtt2 = xtt(:,Index);

[nx,ny] = size(Ipi);

for i=1:N,
  x0 = xtt2(1,i);
  y0 = xtt2(2,i);
  crIx = round(x0);
  crIy = round(y0);

  if (crIx>2+wintx) & (crIx<nx-wintx-1) & ...
	(crIy>2+winty) & (crIy<ny-winty-1),
    
    SI = Ipi(crIx-wintx-2:crIx+wintx+2,...
	     crIy-winty-2:crIy+winty+2);
    
    itIx = x0 - crIx;
    itIy = y0 - crIy;

    if itIx > 0,
      vIx = [itIx 1-itIx 0]';
    else
      vIx = [0 1+itIx -itIx]';
    end;
    if itIy > 0,
      vIy = [itIy 1-itIy 0];
    else
      vIy = [0 1+itIy -itIy];
    end;
    
    SI = conv2(conv2(SI,vIx,'same'),vIy,'same');
    SI = SI(2:2*wintx+4,2:2*winty+4);

    %note the convention
    [gy,gx] = gradient(SI);
    gx = gx(2:2*wintx+2,2:2*winty+2);
    gy = gy(2:2*wintx+2,2:2*winty+2);
    a = sum(sum(gx .* gx));
    b = sum(sum(gx .* gy));
    c = sum(sum(gy .* gy));
    m = (a + c) / 2;
    d = a * c - b ^ 2;
    n = sqrt(m ^ 2 - d);
    Q2(i) = min(abs(m - n),abs(m + n));
  end;
end;

Qtt(Index) = Q2;

