function fhat = roe_flux(um,up)

fm = flux(um); fp = flux(up);
s = zeros(length(um),1);

for i=1:length(um)
    if (up(i) == um(i))
        s(i) = um(i);
    else
        s(i) = (fp(i)-fm(i))./(up(i)-um(i));
    end
end
fhat = 0.5*( fm+fp - sign(s).*(fp-fm) );

return