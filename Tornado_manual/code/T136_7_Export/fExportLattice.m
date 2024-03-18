function [ output_args ] = fExportLattice( lattice )
%This function exports the computational lattice to a CSV textfile

filename='lattice.txt'
[a b]=size(lattice.COLLOC);
[l m n]=size(lattice.VORTEX)

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
    for j=1:3 
        COLL=strcat(COLL,num2str(lattice.COLLOC(i,j)));
        if j<b
            COLL=[COLL ','];
        end
    end
    if i<a
        COLL=[COLL '\n'];
    end
end
COLL=[COLL '\n'];

%% Normals points.

NORM=[];
for i=1:a  
    for j=1:3 
        NORM=strcat(NORM,num2str(lattice.N(i,j)));
        if j<b
            NORM=[NORM ','];
        end
    end
    if i<a
        NORM=[NORM '\n'];
    end
end
NORM=[NORM '\n'];

%% Panel Corner points.

XYZ_x=[];
for i=1:a  
    for j=1:4 
        XYZ_x=strcat(XYZ_x,num2str(lattice.XYZ(i,j,1)));
        if j<4
            XYZ_x=[XYZ_x ','];
        end
    end
    if i<a
        XYZ_x=[XYZ_x '\n'];
    end
end
XYZ_x=[XYZ_x '\n'];

XYZ_y=[];
for i=1:a  
    for j=1:4 
        XYZ_y=strcat(XYZ_y,num2str(lattice.XYZ(i,j,2)));
        if j<4
            XYZ_y=[XYZ_y ','];
        end
    end
    if i<a
        XYZ_y=[XYZ_y '\n'];
    end
end
[XYZ_x '\n'];

XYZ_z=[];
for i=1:a  
    for j=1:4 
        XYZ_z=strcat(XYZ_z,num2str(lattice.XYZ(i,j,3)));
        if j<4
            XYZ_z=[XYZ_z ','];
        end
    end
    if i<a
        XYZ_z=[XYZ_z '\n'];
    end
end
[XYZ_z '\n'];

XYZ=[XYZ_x,XYZ_y,XYZ_z];

%% Vortex points.

V_x=[];
for i=1:a  
    for j=1:m 
        V_x=strcat(V_x,num2str(lattice.XYZ(i,j,1)));
        if j<m
            V_x=[V_x ','];
        end
    end
    if i<a
        V_x=[V_x '\n'];
    end
end
V_x=[V_x '\n'];



fid = fopen(filename,'w');
P=fprintf(fid,STR1);
P=fprintf(fid,STR2);
P=fprintf(fid,STR3);
P=fprintf(fid,STR4);
P=fprintf(fid,STR5);
P=fprintf(fid,' \n');

P=fprintf(fid,STR6);
P=fprintf(fid,NORM);
P=fprintf(fid,STR7);
P=fprintf(fid,COLL);
P=fprintf(fid,STR7);
P=fprintf(fid,XYZ);
fclose(fid);



end

