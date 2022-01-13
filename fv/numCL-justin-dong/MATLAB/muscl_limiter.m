function [um_mod,up_mod] = muscl_limiter(um,up,u)

% gradients to be adjusted
ut  = um - u;
utt = u - circshift(up,1);

ut_m = minmod(ut, circshift(u,-1)-u, u-circshift(u,1));
utt_m = minmod(utt, circshift(u,-1)-u, u-circshift(u,1));

% modify the cell reconstructions using ut and utt
um_mod = u + ut_m;
up_mod = u - utt_m;
up_mod = circshift(up_mod,-1);

return