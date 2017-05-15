% One of the four oriented functions
% based on polar coordinates

figure(1)
[X,Y]=meshgrid(-5:0.1:5, -5:0.1:5);
Z=0*(X+Y);
mesh(X,Y,Z); hold on; %horizontal plane
alpha=pi/4;
xo=0; yo=0; zo=0; 
for R=0:0.1:5,
   zs=sin(R*pi/5); %the symmetrical filter
   for nang=0:2:358,
      phi=(nang*2*pi)/360;
      beta=phi-alpha;  
      z=zs*cos(beta)^3; %asym. product
      cz=abs(z); cr=0;
      if z>0, cr=1; end;
      clr=[cr,0,(1-cz)]; %color coding
      x=R*cos(phi); y=R*sin(phi);
      plot3([xo x],[yo y],[zo z],'Color',clr); hold on;
      xo=x; yo=y; zo=z;
      view(20,30);
   end;
end;  
title('F1 (45º)')
xlabel('X'); ylabel('Y');