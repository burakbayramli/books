function fhat = godunov_flux(um,up)

fhat = zeros(length(um),1);

for i=1:length(um)
    if (um(i)*up(i)<0 && um(i)<=up(i))
        fhat(i) = 0;
    elseif (um(i)*up(i)<0 && um(i)>up(i))
        fhat(i) = max([flux(um(i)) flux(up(i))]);
    elseif (um(i)*up(i)>=0 && um(i)<=up(i))
        fhat(i) = min([flux(um(i)) flux(up(i))]);
    elseif (um(i)*up(i)>=0 && um(i)>up(i))
        fhat(i) = max([flux(um(i)) flux(up(i))]);
    end
end

return