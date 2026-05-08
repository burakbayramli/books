function ss = generate_surfels(p0,v1,dt,dh)
% Catered to lid driven cavity case.
% p0: starting points for every surfel.
% v1: one defining edge of the pgram, common among all pgrams in this case.

ss = [];
for s = 1:size(p0,1)
    % for every surfel
    startp = p0(s,:)';
    ss = [ss, surfel(startp, startp+v1', dh, dt)];
end
    
    
    
    