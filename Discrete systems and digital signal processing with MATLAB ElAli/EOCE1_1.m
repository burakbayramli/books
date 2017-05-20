
	clf	%clear the plotting area
	t=-1:0.05:0;	%discrete time points
	v=t+1;	%given plot
	ft=-fliplr(t);	%reversing time and changing sign
	fv=fliplr(v);	%reversing v
	%generating a 3x2 plot and plotting v verses t
	subplot(3,2,1),plot(t,v,'*')		
	ylabel('v(t)'); %labeling the y axis
	subplot(3,2,2),plot(ft,fv,'*')	
	ylabel('v(-t)'); %labeling the next plot
	st=ft-3;	%shifting by -3
	sv=fv;
	subplot(3,2,3),plot(st,sv,'*')
	ylabel('v(-3-t)');  
	sct=3*t; %changing the range of t
	scv=sct/3+1;	%scaled v
	subplot(3,2,4),plot(sct,scv,'*')
	ylabel('v(t/3)');  
	veven=1/2*[v fv];
	%row of t values extended to ft values
	subplot(3,2,5),plot([t ft],veven,'*')	
	ylabel('veven(t)'), xlabel('t in seconds')  
	vodd=1/2*[v -fv];
	subplot(3,2,6),plot([t ft],vodd,'*')
	ylabel('vodd(t)'), xlabel('t in seconds')  
    