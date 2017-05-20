clf
	t=0:.05:10;
z1=-0.968*exp(-.049*t)-.0327*exp(-2.065*t)+0.011*exp(-9.886*t)+1;
z2=-1.0294*exp(-.049*t)+0.0307*exp(-2.065*t)-0.0013*exp(-9.886*t)+1;
z3=0.097*exp(-.049*t)+0.0041*exp(-2.065*t)-0.1025*exp(-9.886*t);
	plot(t,z1,'*');% for z1
	hold on
	plot(t,z2,'+'); % for z2
	plot(t,z3,'-'); % for z3
	title('Hand simulation for eoce10');
	gtext('z1(t)');
	gtext('z2(t)');
	gtext('z3(t)');
	xlabel('t in seconds')
