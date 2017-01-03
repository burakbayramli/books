
s = 0;

for i=1:n
	c(i) = (2/pi)* trapz(x,sin(i*x) .* f);
	s = s + c(i) * sin(i*x);
end

hold on; plot(x,s,colors(ic)); hold off

ic = ic + 1; if ic > ncolors, ic = 1; end
