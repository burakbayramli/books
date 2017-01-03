function P = lpcBHenc(E,H,W,viz)
% P = lpcBHenc(E,H,W,viz)  Encode LPC residual as buzz/hiss pitch periods
%      E is a residual from LPC encoding.  P is an encoding 
%      which, for every H samples, returns an integer pitch period
%      or 0 for frames judged as noisy.  Pitch is found via autocorrelation 
%      over a window of W points
% 2001-03-19 dpwe@ee.columbia.edu

if nargin < 2
  W = 256;
end
if nargin < 3
  H = W/2;
end
if nargin < 4
  viz = 0;
end


nhops = floor(length(E)/H);

P = zeros(1,nhops);

pmin = 2;
pmax = 127;
pindex = 1+[pmin:pmax];
pdthresh = 0.2;

ee = [zeros(1,W/2),E,zeros(1,W/2)];

for hop = 1:nhops
  
  midpoint = (hop-0.5)*H;

  xx = ee(midpoint+[1:W]);  % Combines with the W/2 padding to be -W/2:W/2
  
  rxx = xcorr(xx);
  rxx = rxx(W+[0:W-1]);

  pk = find(rxx(pindex) == max(rxx(pindex)));
  pd = pindex(pk(1));
  rratio = rxx(pd)/rxx(1);

if viz
  disp(['hop ',num2str(hop),' pd ',num2str(pd),' rrat ',num2str(rratio)]);
  subplot(211); plot(xx);
  subplot(212); plot(rxx); pause
end

  if (rratio > pdthresh)
    P(hop) = pd-1;  % offset the Matlab vector index base
  else
    P(hop) = 0;  % Noisy period
  end
end
