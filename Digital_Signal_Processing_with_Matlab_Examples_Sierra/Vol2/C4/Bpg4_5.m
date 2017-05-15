%A simple symmetrical filter
% using polar coordinates
figure(1)
xo=0; yo=0; zso=0;
for R=0:0.1:5,
   zs=sin(R*pi/5); 
   for nang=0:2:358,
      phi=(nang*2*pi)/360;
      cg=0;
      if zs>0, cg=1; end;
      clr=[zs,(1-zs),zs]; %color coding
      x=R*cos(phi); y=R*sin(phi);
      plot3([xo x],[yo y],[zso zs],'Color',clr); hold on;
      xo=x; yo=y; zso=zs;
     view(20,50);    
   end;
end;  
title('Symmetrical filter')
xlabel('X'); ylabel('Y');
