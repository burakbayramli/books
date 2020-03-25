function s=stratified_resample(p,N)
% input:  [n,1] vector of resampling probabilities prob=[p1;...;pn]
%               where p(i) corresponds to the probability
%               of resampling the i-th particle;
%        scalar 'N' denoting the number of required samples;
% output: [N,1] vector 's' which gives the
%               indices (ranging from 1 to n) of the
%               particles forming the new sample;
p=p(:);n=size(p,1);
%normalize any weights so that they are proper probabilities
p=p/sum(p);
c=floor(N*p);
s=zeros(N,1);  %preallocate memory
%  deterministic resampling (step 1)
cum=0; % cumulative sum of deterministic copies
for i=1:n
    det_copy=c(i);
    s(cum+1:cum+det_copy)=i*ones(det_copy,1);
    cum=cum+det_copy;
end
N_r=N-sum(c); % residual number
% start step 2
if N_r~=0 % perform residual sampling if needed
    s(N-N_r+1:N)=resample(n,N_r);
end
% destroy the structure induced by the construction of s
% by randomly reallocating the indices of s;
s=s(randperm(N)); %step 3