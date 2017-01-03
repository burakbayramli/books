% Water sprinkler Bayes net
%   C
%  / \
% v  v
% S  R
%  \/
%  v
%  W

C = 1; S = 2; R = 3; W = 4;
false = 1; true = 2;

% Specify the conditional probability tables as cell arrays
% The left-most index toggles fastest, so entries are stored in this order:
% (1,1,1), (2,1,1), (1,2,1), (2,2,1), etc. 

CPD{C} = reshape([0.5 0.5], 2, 1);
CPD{R} = reshape([0.8 0.2 0.2 0.8], 2, 2);
CPD{S} = reshape([0.5 0.9 0.5 0.1], 2, 2);
CPD{W} = reshape([1 0.1 0.1 0.01 0 0.9 0.9 0.99], 2, 2, 2);

% naive method 
joint = zeros(2,2,2,2);
for c=1:2
  for r=1:2
    for s=1:2
      for w=1:2
	joint(c,s,r,w) = CPD{C}(c) * CPD{S}(c,s) * CPD{R}(c,r) *  CPD{W}(s,r,w);
      end
    end
  end
end

% vectorized method
joint2 = repmat(reshape(CPD{C}, [2 1 1 1]), [1 2 2 2]) .* ...
	 repmat(reshape(CPD{S}, [2 2 1 1]), [1 1 2 2]) .* ...
	 repmat(reshape(CPD{R}, [2 1 2 1]), [1 2 1 2]) .* ...
	 repmat(reshape(CPD{W}, [1 2 2 2]), [2 1 1 1]);

assert(approxeq(joint, joint2));

pSandW = sumv(joint(:,true,:,true), [C R]); % 0.2781
pW = sumv(joint(:,:,:,true), [C S R]); % 0.6471
pSgivenW = pSandW / pW;  % 0.4298

pRandW = sumv(joint(:,:,true,true), [C S]); % 0.4581
pRgivenW = pRandW / pW; % 0.7079

% P(R=t|W=t) > P(S=t|W=t), so
% Rain more likely to cause the wet grass than the sprinkler

pSandRandW = sumv(joint(:,true,true,true), [C]); % 0.0891
pSgivenWR = pSandRandW / pRandW; % 0.1945

% P(S=t|W=t,R=t) << P(S=t|W=t)
% Sprinkler is less likely to be on if we know that
% it is raining, since the rain can "explain away" the fact
% that the grass is wet.

