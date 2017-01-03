function E = lpcBHdec(P,H);
% E = lpcBHdec(P,H)  Decode LPC residual encoded as pitch periods
%      P is a vector pitch periods from lpcresenc.  Reconstruct a 
%      stylized excitation vector E with a hop size H.
% 2001-03-19 dpwe@ee.columbia.edu

if nargin < 2
  H = 128;
end

nhops = length(P);
npts = H*nhops;

E = zeros(1,npts);

phs = 0;   % Current phase as proportion of a cyle (new pulse at 1.0)

for hop = 1:nhops
  
  pd = P(hop);
  base = H*(hop-1);
  
  if (pd == 0)
    
    E(base+[1:H]) = randn(1,H);
    
  else
    
    pt = 0;
    % Steps to next pulse
    remsteps = round((1-phs) * pd);
    
    while (pt + remsteps) < H
      pt = pt + remsteps;
      E(base+1+pt) = sqrt(pd);  % so rms is 1
      remsteps = pd;
    end      
    
    % Store residual phase
    phs = (H - pt)/pd;
    
  end
  
end

    