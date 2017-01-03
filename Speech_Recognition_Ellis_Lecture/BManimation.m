M=20;	%The number of frames/cycle in the animation
deltaT=1/(M*f);	%Time step for animation 

xmin=0;					%Figure out what limits to use on Y-axis
xmax=max(x);

ymax=max(norm_Vbm);	%Figure out what limits to use on Y-axis
ymin=-ymax;

figure;
shg;
%	waveform=norm_Vbm.*cos(theta_Vbm);
%	plot(x,waveform);
	MovieMatrix=moviein(M);
for jj=1:M	%animate for cycle
	waveform=norm_Vbm.*cos(w*jj*deltaT + theta_Vbm);
   plot(x,waveform);
   axis([xmin xmax ymin ymax]);
   grid;
   xlabel('Distance from base (cm)');
   ylabel('Basilar Membrane Displacement');

   MovieMatrix(:,jj) = getframe;

end

movie(MovieMatrix,20);