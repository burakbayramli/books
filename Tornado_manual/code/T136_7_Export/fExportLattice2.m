function [ output_args ] = fExportLattice( lattice )
%This function exports the computational lattice to a textfile

filename='lattice.txt';
[a b]=size(lattice.COLLOC);
[l m n]=size(lattice.VORTEX);

STR1='%% TORNADO COMPUTATIONAL LATTICE                     \n';
STR2='%% Order of items: Number of panels, scalar  double] [x,y,z] coords.  \n';
STR3='%%                 Normals             [n,3] double. [x,y,z] coords.  \n';
STR4='%%                 Collocation points  [n,3] double. [x,y,z] coords.  \n';
STR5='%%                 Panel corner points [n,4] double. Panel x-coords.  \n';
STR6='%%                 Panel corner points [n,4] double. Panel y-coords.  \n';
STR7='%%                 Panel corner points [n,4] double. Panel z-coords.  \n';

STR6=strcat(num2str(a), '\n');
STR7=['%% \n'];


%% Collocation points.

COLL=[];
for i=1:a  
        COLL=strcat(COLL,num2str(lattice.COLLOC(i,1)),032,num2str(lattice.COLLOC(i,2)),032,num2str(lattice.COLLOC(i,3)),'\n');
end


%% Normals points.

NORM=[];
for i=1:a  
        NORM=strcat(NORM,num2str(lattice.N(i,1)),032,num2str(lattice.N(i,2)),032,num2str(lattice.N(i,3)),'\n');
end


%% Panel Corner points.

XYZ=[];
for i=1:a  
    for j=1:4 
        XYZ=strcat(XYZ,num2str(lattice.XYZ(i,j,1)),032,num2str(lattice.XYZ(i,j,2)),032,num2str(lattice.XYZ(i,j,3)),'\n');
    end
end


%% Vortex points.

V=[];
for i=1:a  
    for j=1:4 
        V=strcat(V,num2str(lattice.VORTEX(i,j,1)),032,num2str(lattice.VORTEX(i,j,2)),032,num2str(lattice.VORTEX(i,j,3)),'\n');
    end
end



fid = fopen(filename,'w');
P=fprintf(fid,STR6);
P=fprintf(fid,COLL);
P=fprintf(fid,NORM);
P=fprintf(fid,XYZ);
P=fprintf(fid,V);
fclose(fid);



end

