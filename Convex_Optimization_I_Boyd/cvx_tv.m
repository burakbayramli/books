cvx_begin
   variable Utv(m, n);
   Utv(Known) == Uorig(Known); % Fix known pixel values.
   Ux = Utv(2:end,2:end) - Utv(2:end,1:end-1); % x (horiz) differences
   Uy = Utv(2:end,2:end) - Utv(1:end-1,2:end); % y (vert) differences
   minimize(norm([Ux(:); Uy(:)], 1)); % tv roughness measure
cvx_end
