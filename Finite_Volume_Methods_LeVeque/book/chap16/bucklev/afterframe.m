axis([-0.5 1.5 -0.2 1.2])
hold on

% true solution 

a = .5;
qleft = 1;
qright = 0;
qs = sqrt(a/(1+a));
fs = qs^2 / (qs^2 + a*(1-qs)^2);
s1 = fs/qs;  % shock speed

qrare = linspace(1,qs,100);
srare = 2*a*qrare.*(1-qrare) ./ ((qrare.^2 + a*(1-qrare).^2).^2);
xrare = t*srare;
xt1 = [-1 0 xrare s1*t 3];
qt1 = [qleft qleft qrare qright qright];
hline = plot(xt1,qt1);
%set(hline,'LineWidth',2);

hold off

