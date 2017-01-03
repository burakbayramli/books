
a0 = trapz(x,f)/(2 * pi);

s = a0 * ones(1,m);

for i=1:n
	a(i) = trapz(x,cos(i*x) .* f)/pi;
	s = s + a(i) * cos(i*x);
	b(i) = trapz(x,sin(i*x) .* f)/pi;
	s = s + b(i) * sin(i*x);
end

hold on; plot(x,s,colors(ic)); hold off

ic = ic + 1; if ic > ncolors, ic = 1; end
