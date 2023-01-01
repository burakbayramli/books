%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 1 - project 11
%%   THER: Thermic engineering
%%   Solution of the direct problem : computation of the
%%   temperature fields when thermic resistances are known
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
   clear all ; close all ;
%  Geometry
%% Coarse mesh "mesh.coarse" : 173 vertices and  304 triangles
   Filename='mesh.coarse';
%  Resistances coordinates
   Pres=[-.80 .80;   .80 .80; -.85 -.80;   .85 -.80 ];              %  4 resistances 
%% Pres=[-.75 .75; 0. .75; .75 .75; -.75 -.75; 0. -.75; .75 -.75 ]; %  6 resistances 
%  Heat conductivity coefficients
   Conduc=[10. 1.]; 
%  Boundary conditions 
   Refdir=[4 ; 3];Valdir=[100 ; 50 ];
%  Temperature fields  
   [T0,Tres,Numtri,Coorpt]=THER_oven(Refdir,Valdir,Conduc,Pres,Filename); 
%  Plot of results
   nf=1;figure(nf);fs=18;
   trisurf(Numtri,Coorpt(:,1),Coorpt(:,2),T0); 
   title('Homegeneous temperature field','FontSize',fs);
   nf=size(Pres,1);
   for k=1:nf 
   nft=k+1;figure(nft); 
   trisurf(Numtri,Coorpt(:,1),Coorpt(:,2),Tres(:,k));
   text1='Temperature field for resistance n';text2=int2str(k);
   text=strcat(text1,text2);
   title(text,'FontSize',fs);
   end
   fid = fopen('temp.end','w');
   save fid T0 Tres