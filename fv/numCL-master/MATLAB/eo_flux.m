function fhat = eo_flux(um,up)

fhat = zeros(length(um),1);

for i=1:length(um)
    if (um(i)>0)
        a = flux(um(i));
    else
        a = 0;
    end

    if (up(i)>0)
        b = 0;
    else
        b = flux(up(i));
    end

    fhat(i) = a+b;
end

return