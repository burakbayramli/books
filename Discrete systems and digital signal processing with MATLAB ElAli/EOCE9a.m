clf
	y=0:0.01:4;
	fy=1-sqrt(y);
	deltafy=-1/2*y;
	fyapprox=deltafy+1/2;
	plot(y,fy,y,fyapprox)
	xlabel('y');
	title('f(y) and f(y) approximated at y=1');
	gtext('f(y)')
	gtext('f(y) approximated')
