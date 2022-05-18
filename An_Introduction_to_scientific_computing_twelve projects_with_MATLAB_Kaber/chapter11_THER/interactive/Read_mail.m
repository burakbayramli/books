%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%==================================================
% Reads the mesh from the given file 
%  (special .ambda format)
%===================================================


function Read_mail

global Ns Nt XYs Refs I123 Reft Nomfic

fid=fopen(Nomfic,'r');

tmp=fscanf(fid,'%i');            % first line
Ns=tmp(1);Nt=tmp(2);

line=fgets(fid);                 % go to the next line

% reads the vertices
tmp=fscanf(fid,'%f',[4,Ns]);     % reads 4*Ns elements -> matrix of 4 lines and Ns columns
XYs =tmp(2:3,:)';
Refs=tmp(4,:)';

% reads the triangles
tmp=fscanf(fid,'%i',[5,Nt]);     % reads 5*Nt elements -> matrix of 5 lines and Nt columns
I123=tmp(2:4,:)';
Reft=tmp(5,:)';

fclose(fid);
