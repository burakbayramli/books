%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%==================================================
% Reads the mesh from the given file 
%  (special freefem format)
%===================================================


function Read_mail_freefem

global Ns Nt XYs Refs I123 Reft Nomfic

fid=fopen(Nomfic,'r');

tmp=fscanf(fid,'%i',3);            % first line
Ns=tmp(1);Nt=tmp(2);

% reads the vertices
tmp=fscanf(fid,'%f',[3,Ns]);     % reads 4*Ns elements -> matrix of 4 lines and Ns columns
XYs =tmp(1:2,:)';
Refs=tmp(3,:)';

% reads the triangles
tmp=fscanf(fid,'%i',[4,Nt]);     % reads 5*Nt elements -> matrix of 5 lines and Nt columns
I123=tmp(1:3,:)';
Reft=tmp(4,:)';Reft=Reft+1; % to avoid zero labels

fclose(fid);
