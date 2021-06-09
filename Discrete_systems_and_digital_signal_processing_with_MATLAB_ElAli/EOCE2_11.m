k=input('enter a value for k, a negative number to exit:');
	t=0:0.5:100;	
	while k > 0
   	n=[1];
   	d=[1 k 1];
   	ystep=step(n,d,t);
   	yimpulse=impulse(n,d,t);
   	plot(t,ystep,'*',t,yimpulse,'+')
   	title('The step and the impulse response')
		gtext('step')
		gtext('impulse')
		% … is used for continuation on the next line
   	k=input('enter a value for k, a negative number to exit:');
		end
	xlabel('t in seconds');
