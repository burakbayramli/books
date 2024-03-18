function [results]=trefftz(results,state,geo,lattice,ref)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trefftz: Subsidiary function for TORNADO						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Computes and plots constituents of Trefftz plane analysis drag.		 
%	         															
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Author:	Tomas Melin, University of Bristol
%           Aeronautical Engineering Department	
%		      copyright 2007											
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTEXT:	Subsidary function for TORNADO					
% Called by:	main->postproc->resultplot													
% Calls:	    MATLAB  std fcns
%
%																			
% Loads: none										
% Saves: none															
% Input: results,state,geo,lattice,ref															
% Output:results														
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Rudimentary trefftz plane analysis for Tornado
%
% It works as is, but users may want to experiment by
% Changing the panelling, "npan", and the width of the integration area
% "span_multiplier". This is done by hardcoding this file.
%
% The code takes the y and z position of all trailing edge vorticies
% both the incoming and outgoing of each panel horseshoe, then employs the
% Muncks stagger theoreme to compute the induced velocities in the trefftz
% plane (infinitely far downstream). From the velocities comes energy rate
% and thus Drag. The drag is then numerically integrated throughout the
% trefftz plane.
%
% The size of the treftz plane, i.e. the integration limits, are
% automagically adapted to the reference span of the AC. -Should the span 
% be much smaller than the height, the code will fail. Also, is is a bit
% sensitive to the size of the incremental steps. Especially if the middle
% of a dx x dy square comes very close to a vortex line.
%
% Notably, there is a difference between the vector analysis drag, and the
% trefftz plane drag. I don't know why yet, so if you know, please do tell.

%tic


npan=56;                        %Number of panels in treftz plane

[a1 a2 a3]=size(lattice.VORTEX);
arrowsize=5;

for i=1:a1
      vpos(i,1,1)=lattice.VORTEX(i,2,2);  %y pos incoming
      vpos(i,2,1)=lattice.VORTEX(i,2,3);   %z pos        
      vpos(i,1,2)=lattice.VORTEX(i,7,2);  %y pos outgoing
      vpos(i,2,2)=lattice.VORTEX(i,7,3);   %z pos 
end

span_multiplier=1;

zmax=span_multiplier*ref.b_ref;
ymax=span_multiplier*ref.b_ref;


dl=2*zmax/npan;

%Numerical drag iteration

for a=0:npan-1         %  Rows
    for b=0:npan-1    %  Cols
        V=0;
        %for c=1:a1   %  Per vortex

            %ingoing
            ry=(ymax-dl/2)-a*dl-vpos(:,1,1);
            rz=(zmax-dl/2)-b*dl-vpos(:,2,1);
            
            r=[rz, -ry];
            lr=sqrt(sum(r.^2,2));
            er=r(:,1)./lr;
            er(:,2)=r(:,2)./lr;
            
            v1=results.gamma(:,1)./(2*pi*lr).*er(:,1);
            v1(:,2)=results.gamma(:,1)./(2*pi*lr).*er(:,2);
            
            
            %outgoing
            ry=(ymax-dl/2)-a*dl-vpos(:,1,2);
            rz=(zmax-dl/2)-b*dl-vpos(:,2,2);
            
            r=[rz, -ry];
            lr=sqrt(sum(r.^2,2));
                
            er=r(:,1)./lr;
            er(:,2)=r(:,2)./lr;
            
            v2=-results.gamma(:,1)./(2*pi*lr).*er(:,1);
            v2(:,2)=-results.gamma(:,1)./(2*pi*lr).*er(:,2);
            
            V=sum([v1;v2]);
            
            
            

        %end
       x(a+1,b+1)=(ymax-dl/2)-a*dl;
       y(a+1,b+1)=(zmax-dl/2)-b*dl;
       
       VVEC(a+1,b+1,:)=V; 
       VTOT(a+1,b+1)=sqrt(sum(V.^2));
       
       D(a+1,b+1)=0.5*state.rho*(sum(V.^2))*dl^2;
       
       
    end
end

results.treffts.x=x;
results.treffts.y=y;
results.treffts.D=D;
results.treffts.VTOT=VTOT;
results.treffts.VVEC=VVEC;


Trefftz_drag_Coeff=sum(sum(D))/(0.5*state.rho*state.AS^2*ref.S_ref);
results.Trefftz_drag_Coeff=Trefftz_drag_Coeff;


%return
%Moving these to resultplot
figure(23)
surf(x,y,D)
xlabel('y coordinate, [m]')
ylabel('z coordinate, [m]')
zlabel('Distributed drag contrubution, [N]')
title('Treffz plane drag contribution field.')


figure(22)
contour(x,y,VTOT,20), hold on
quiver(x,y,VVEC(:,:,1),VVEC(:,:,2),arrowsize)
xlabel('y coordinate, [m]')
ylabel('z coordinate, [m]')
title('Treffz plane velocity vector field.')
%   toc

