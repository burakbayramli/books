function [um_mod,up_mod] = tvb_limiter(um,up,u,dx,M)

% gradients to be adjusted
ut  = um - u;
utt = u - circshift(up,1);

ut_m = minmod_tvb(ut, circshift(u,-1)-u, u-circshift(u,1), dx, M);
utt_m = minmod_tvb(utt, circshift(u,-1)-u, u-circshift(u,1), dx, M);

% modify the cell reconstructions using ut and utt
um_mod = u + ut_m;
up_mod = u - utt_m;
up_mod = circshift(up_mod,-1);

return