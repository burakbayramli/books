function E = lpcMPEdec(T,M,H)
% E = lpcMPEdec(T,M,H)  Reconstruct excitation from MPE encoding
%     T, M are time and size defs of pulses from lpcmpeenc.
%     H is the hop size (128).
% 2001-03-20 dpwe@ee.columbia.edu

if nargin < 3
  H = 128;
end

[nhops, npuls] = size(T);

E = zeros(1,nhops*H);

for hop = 1:nhops

  base = (hop-1)*H;

  tt = T(hop,:);
  mm = M(hop,:);

  E(base +  tt) = mm;

end
