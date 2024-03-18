%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1999, 2007 Tomas Melin
%
% This file is part of Tornado
%
% Tornado is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public
% License as published by the Free Software Foundation;
% either version 2, or (at your option) any later version.
%
% Tornado is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied
% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE.  See the GNU General Public License for more
% details.
%
% You should have received a copy of the GNU General Public
% License along with Tornado; see the file GNU GENERAL 
% PUBLIC LICENSE.TXT.  If not, write to the Free Software 
% Foundation, 59 Temple Place -Suite 330, Boston, MA
% 02111-1307, USA.
%
% usage: [-]=GEOMETRYPLOT(LATTICE,GEO,REF)
%
%   geometryplot plots the computational lattice inf figures 1,2,3 and 50.
%   It provides a good way for the user to check the consistensy of the
%   LATTICE strucure which contains vortex points, panel corner points
%   panel normals and panel collocation points. Additionally, the GEO and
%   REF structs are needed to plot the reference point and mean aerodunami
%   chord.
%
% Example:
%
%  [lattice,ref]=fLattice_setup(geo,state);
%  geometryplot(lattice,geo,ref);
%
% Calls:
%      None.
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado text based user interface
%
% Revision History:
%   Bristol,    2007 06 27:  Addition of new header. T.M.
%   Stockholm,  2002 02 10:_ Graphical C_mac and ref pos. TM.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function[]=geometryplot(lattice,geo,ref);

[t q2 q3]=size(lattice.VORTEX);

figure(1)
%Changing variables to plot only partition outline
g2=geo;
g2.nx=double(g2.nx>0);
g2.ny=double(g2.ny>0); 
g2.fnx=double(g2.fnx>0); 
s2.AS=1;
s2.alpha=0;
s2.betha=0;
s2.P=0;
s2.Q=0;
s2.R=0;
s2.ALT=0;
s2.rho=1;
s2.pgcorr=0;

[l2,ref]=fLattice_setup2(g2,s2,1);

g=fill3(l2.XYZ(:,:,1)',l2.XYZ(:,:,2)',l2.XYZ(:,:,3)','w');
set(g,'LineWidth',2);
view([0,90]);
axis equal,hold on
xlabel('Aircraft body x-coordinate')
ylabel('Aircraft body y-coordinate')
zlabel('Aircraft body z-coordinate')
title('3D wing and partition layout')
grid on

h=line([ref.mac_pos(1) ref.mac_pos(1)+ref.C_mac],[ref.mac_pos(2) ref.mac_pos(2)],[ref.mac_pos(3) ref.mac_pos(3)]);
set(h,'LineWidth',5);

a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'r+');
set(a,'MarkerSize',15,'linewidth',3);
a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'ro');
set(a,'MarkerSize',15,'linewidth',3);

a=plot3(geo.CG(1),geo.CG(2),geo.CG(3),'ko');
set(a,'MarkerSize',15,'linewidth',3);
a=plot3(geo.CG(1),geo.CG(2),geo.CG(3),'kx');
set(a,'MarkerSize',15,'linewidth',3);

%plotting legend
L=gca;
set(L,'Position',[0.1 0.1 0.6 0.8]);
axes('position',[0.75 0.6 0.2 0.2]);
axis([0 1 0 1])
hold on
h=line([0.1 0.4],[1 1]);
set(h,'LineWidth',6);

a=plot(0.25,0.66,'r+');
set(a,'MarkerSize',15,'linewidth',3);
a=plot(0.25,0.66,'ro');
set(a,'MarkerSize',15,'linewidth',3);

a=plot(0.25,0.33,'kx');
set(a,'MarkerSize',15,'linewidth',3);
a=plot(0.25,0.33,'ko');
set(a,'MarkerSize',15,'linewidth',3);

text(0.5,1,'MAC');
text(0.5,0.66,'ref point')
text(0.5,0.33,'c.g.')


axis off

if isfield(geo,'prop')
    figure(101)   %Propeller plot
    g=fill3(l2.XYZ(:,:,1)',l2.XYZ(:,:,2)',l2.XYZ(:,:,3)','w');
    set(g,'LineWidth',2);
    view([0,90]);
    axis equal,hold on
    xlabel('Aircraft body x-coordinate')
    ylabel('Aircraft body y-coordinate')
    zlabel('Aircraft body z-coordinate')
    title('3D wing and partition layout')
    grid on

    propplot(lattice.prop)
    discplot( geo.prop.dia,geo.prop.pos );
    
    
       [a b]=size(geo.prop.pos);
  for i=1:a
       if geo.prop.Psym(i)==1
           discplot( geo.prop.dia(i),[1 -1 1].*geo.prop.pos(i,:) );
       
       
       end
    
    end
    
    
    view([-2 -1 1])                                
end




try
    B=lattice.XYZ(:,:,3);           %Check if geometry is present.
catch
    terror(10);
    return
end


h=figure(50);
set(gca,'Position',[0 0 1 1]);
set(h,'Color','White');


subplot(2,2,1);
    h2=plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','k');
    set(gca,'Position',[0.05 0.55 0.40 0.40]);
    axis equal,hold on,axis off, view([0 0]);
    title('Side')
    grid on
    h=line([ref.mac_pos(1) ref.mac_pos(1)+ref.C_mac],[ref.mac_pos(2) ref.mac_pos(2)],[ref.mac_pos(3) ref.mac_pos(3)]);
    set(h,'LineWidth',5);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'r+');
    set(a,'MarkerSize',15);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'ro');
    set(a,'MarkerSize',15);
  
subplot(2,2,2);
    h2=plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','k');
    set(gca,'Position',[0.55 0.55 0.40 0.40]);
    axis equal,hold on,axis off,view([-90 0]);
    title('Front')
    grid on
    h=line([ref.mac_pos(1) ref.mac_pos(1)+ref.C_mac],[ref.mac_pos(2) ref.mac_pos(2)],[ref.mac_pos(3) ref.mac_pos(3)]);
    set(h,'LineWidth',5);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'r+');
    set(a,'MarkerSize',15);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'ro');
    set(a,'MarkerSize',15);

subplot(2,2,3)
    h2=plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','k');
    set(gca,'Position',[0.025 0.025 0.45 0.45]);
    axis equal,hold on,axis off,view([0 90]);
    title('Top')
    grid on
    h=line([ref.mac_pos(1) ref.mac_pos(1)+ref.C_mac],[ref.mac_pos(2) ref.mac_pos(2)],[ref.mac_pos(3) ref.mac_pos(3)]);
    set(h,'LineWidth',5);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'r+');
    set(a,'MarkerSize',15);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'ro');
    set(a,'MarkerSize',15);

subplot(2,2,4)
    h2=plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','k');
    set(gca,'Position',[0.525 0.025 0.45 0.45]);
    axis equal,hold on, axis off,view([45 45]);
    title('ISO')
    grid on
    h=line([ref.mac_pos(1) ref.mac_pos(1)+ref.C_mac],[ref.mac_pos(2) ref.mac_pos(2)],[ref.mac_pos(3) ref.mac_pos(3)]);
    set(h,'LineWidth',5);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'r+');
    set(a,'MarkerSize',15,'linewidth',3);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'ro');
    set(a,'MarkerSize',15);
   
    %a=text(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'Reference point');
    
    %try
    %        a=plot3(geo.CG(1),geo.CG(2),geo.CG(3),'kx');
    %        set(a,'MarkerSize',15);
    %         a=plot3(geo.CG(1),geo.CG(2),geo.CG(3),'ko');
    %         set(a,'MarkerSize',15);
    %         a=text(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'Center of gravity');
    %         
    %end
    
    figure(51)
    h2=plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','k');
    %set(gca,'Position',[0.025 0.025 0.45 0.45]);
    axis equal,hold on,view([0 90]), grid off;
    title('Top view')
    grid on
    h=line([ref.mac_pos(1) ref.mac_pos(1)+ref.C_mac],[ref.mac_pos(2) ref.mac_pos(2)],[ref.mac_pos(3) ref.mac_pos(3)]);
    set(h,'LineWidth',5);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'r+');
    set(a,'MarkerSize',15,'linewidth',3);
    a=plot3(geo.ref_point(1),geo.ref_point(2),geo.ref_point(3),'ro');
    set(a,'MarkerSize',15,'linewidth',3);
    
    a=plot3(geo.CG(1),geo.CG(2),geo.CG(3),'ko');
    set(a,'MarkerSize',15,'linewidth',3);
    a=plot3(geo.CG(1),geo.CG(2),geo.CG(3),'kx');
    set(a,'MarkerSize',15,'linewidth',3);
    xlabel('Body X coordinate')
    ylabel('Body Y coordinate')
    
    
figure(2)
plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','m')
hold on
grid on  
for s=1:(t)	
   w=0;
   for u=1:q2-1
      w=w+1;
      VX(w,:)=[lattice.VORTEX(s,u,1) lattice.VORTEX(s,u+1,1)];
      VY(w,:)=[lattice.VORTEX(s,u,2) lattice.VORTEX(s,u+1,2)];
      VZ(w,:)=[lattice.VORTEX(s,u,3) lattice.VORTEX(s,u+1,3)];
	end   
   	rc=lattice.COLLOC(s,:);
   	A=rc+lattice.N(s,:);					%Check routine
      x=[rc(1) A(1)];				%Calculating normals
      y=[rc(2) A(2)];				         
      z=[rc(3) A(3)];
         
      NORMALS(s,:,1)=x;				%saving normals
      NORMALS(s,:,2)=y;
      NORMALS(s,:,3)=z;
      
      plot3(VX,VY,VZ,'r.-.')
 
   end
xlabel('Body x-coord')
ylabel('Body y-coord')
zlabel('Body z-coord')
title('3D wing configuration, vortex and wake layout.')
axis equal 

figure(3)
plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','k')
hold on

for s=1:(t)
   plot3(lattice.COLLOC(s,1),lattice.COLLOC(s,2),lattice.COLLOC(s,3),'g*')
   plot3(NORMALS(s,:,1),NORMALS(s,:,2),NORMALS(s,:,3),'r:');
end

axis equal
xlabel('Body x-coordinate')
ylabel('Body y-coordinate')
zlabel('Body z-coordinate')
title('3D panels, collocation points and normals.')
grid on
end


function [out]=propplot(data);

[a b c]=size(data.XYZ);

for i=1:a
    h=plot3(data.XYZ(i,:,1),data.XYZ(i,:,2),data.XYZ(i,:,3),'k');
    set(h,'LineWidth',0.5);
    axis equal;
    hold all;
end

end

function [ out ] =discplot( D,Origin )
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here


[a b]=size(Origin);

for i=1:a
    %UGLY HACK BELOW!
    phi=0:0.01:2*pi;
    x=zeros(size(phi))+Origin(i,1);
    y=D(i)/2*cos(phi)+Origin(i,2);
    z=D(i)/2*sin(phi)+Origin(i,3);
    



    h=plot3(x,y,z,'k');
    set(h,'LineWidth',2);

    h=fill3(x,y,z,'blue');
    set(h,'FaceAlpha',0.2);

out=[];
end
end