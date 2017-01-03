function [T,M]=lpcMPEenc(A,E,N,H,G)
% [T,M] = lpcMPEenc(A,E,N,H,G)    Encode LPC residual with multi-pulses
%     A and E are the filter and excitation, respectively, returned by 
%     lpcfit.  H is the hopsize in samples (128).  Find an N-pulse (default 10) 
%     approximation to E for each frame, using a perceptual filter weighted 
%     by G (default 0.8).  Each row of T specifies N sample indices, and 
%     each row of M gives the magnitudes of impulses at those times.
% 2001-03-20 dpwe@ee.columbia.edu

if nargin < 3
  N = 10;
end
if nargin < 4
  H = 128;
end
if nargin < 5
  G = 0.8;
end

% How many points to use for pcp wt filter
irlen = 40;

[nhops, ncols] = size(A);

T = zeros(nhops, N);
M = zeros(nhops, N);

for hop = 1:nhops;

  base = (hop-1)*H;
  efrag = E(base+[1:H]);

  % Figure IR approx to pcp wt filter bit
  ag = [1, A(hop, 2:ncols).*(G.^(1:(ncols-1)))];
  hgamma = filter(1,ag,[1, zeros(1,(irlen - 1))]);

  % Apply pcp wt to Efrag
  wte = filter(1,ag,[efrag, zeros(1,(irlen))]);
  wteorig = wte;
  esynth = zeros(1,H);

  % Iteratively find pulses
  ptsdone = 0;
  while ptsdone < N

    rhw = conv(fliplr(hgamma), wte);

    % Limit to acceptable range
    rhw = rhw(length(hgamma) + [0:(H-1)]);

    pos = find(abs(rhw) == max(abs(rhw)));
    val = rhw(pos)/sum(hgamma.^2);

    if esynth(pos) ~= 0
        % Updating an existing point
        oldval = esynth(pos);

        %disp(['hop=',num2str(hop),' pt=',num2str(pt),' degenerate pos=',num2str(pos), ' val=', num2str(oldval),'->',num2str(val)]);
	%plot([1:length(wte)], wte, pos - 1 + [1:length(hgamma)], val*hgamma);
	%pause;

        oldpt = find(T(hop,:) == pos);
        % Update the values of the old point
	M(hop,oldpt) = oldval + val;
        esynth(pos) = oldval+val;
    else
        % It's a new point, so add a new pulse to our list
        ptsdone = ptsdone + 1;
        T(hop, ptsdone) = pos;
        M(hop, ptsdone) = val;
        esynth(pos) = val;
    end

    % Subtract from the residual
    wte(pos - 1 + [1:length(hgamma)]) = wte(pos - 1 + [1:length(hgamma)]) - val*hgamma;

  end

%  subplot(211)
%  plot(1:H,wteorig(1:H),1:H,wte(1:H),'r');
%  subplot(212)
%  plot(1:H, efrag, 1:H, esynth, 'r');
%  pause

end
