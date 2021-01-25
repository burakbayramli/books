%Reads density and velocity .dat files from LB2D_Prime (select output of these in Flags.h) and
%plots pressure contours and velocity vectors.
%Input file names and problem dimensions must be specified below.

clear all
close all


%Change to correct file
rho=load('rho34x198_frame0010_subs00_proc0000.dat')
p_vector=rho/3

%Change to problem dimensions (rows = x_max, columns = y_max, both from
%file name)

rows=198
columns=34

%skip factor for plot to reduce plot density; use 1 for all points
rowskip=1
colskip=1

for j=1:columns
    j
    for i=1:rows
        p(i,j)=p_vector(j+(i-1)*columns);  
        
        %Get rid of '0 pressure' solids that dominate pressure field
        if p(i,j)==0
             p(i,j)=NaN;
        end
    end
end

[C,H]=contour(p(1:rowskip:rows,1:colskip:columns));
clabel(C,H);
axis equal


%Change to correct file

uv_vector=load('u34x198_frame0010_subs00_proc0000.dat')


for j=1:columns
    j
    for i=1:rows
        u(i,j)=uv_vector(j+(i-1)*columns,1);  
        v(i,j)=uv_vector(j+(i-1)*columns,2);  

    end
end

hold on
quiver(u(1:rowskip:rows,1:colskip:columns),v(1:rowskip:rows,1:colskip:columns))

%This is for streamlines starting at y = rows from x = 1 to columns along the x axis. 
%Different geometries will require different starting points.

[Stream]= stream2(u,v,[1:columns],rows*ones(columns,1));
streamline(Stream)