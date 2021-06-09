clf
	num1=[1 1];
	den1=[1 1 1];
	num2=[1 0];
	den2=[1 1];
	num=conv(num1,num2); % multiply num1 with num2
	den=conv(den1,den2);
	bode(num,den);
	title('The filter representing H1(s)H2(s)')
