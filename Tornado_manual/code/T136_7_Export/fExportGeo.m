function [ output_args ] = fExportGeo(geo)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here


        fname=('geometry1.txt');
        fid=fopen(fname,'w');

        nw=geo.nwing;
        np=max(geo.nelem);
        
        
        
        fprintf(fid,'%%Tornado exported geometry data. \n');
        fprintf(fid,strcat(strcat('%%Date: ', date), '\n'));
        fprintf(fid,'%%--------------------------------------------- \n');
        
        fprintf(fid,'%% Geometry version \n');
            fprintf(fid,'%i \n',geo.version);
        fprintf(fid,'%% Geometry Name \n');
            fprintf(fid,'%s \n',geo.name);
        fprintf(fid,'%% Geometry project \n');
            fprintf(fid,'%c \n','0');        
        fprintf(fid,'%% Center of gravity: \n');
            fprintf(fid,'%6f %6f %6f \n', geo.CG(1),  geo.CG(2) , geo.CG(3));       
        fprintf(fid,'%% Reference point position \n');    
            fprintf(fid,'%6f %6f %6f \n', geo.ref_point(1),  geo.ref_point(2) , geo.ref_point(3));       
        fprintf(fid,'%% Number of Wings \n');   
            fprintf(fid,'%i \n', geo.nwing);     
        fprintf(fid,'%% Number of partitions per wing \n');
            for i=1:nw;
                fprintf(fid,'%i ', geo.nelem(i));           
            end    
            fprintf(fid, ' \n');  
        fprintf(fid,'%% Wing symmetry set bit. \n');
            for i=1:nw;
                fprintf(fid,'%i ', geo.symetric(i));           
            end
            fprintf(fid, ' \n'); 
        fprintf(fid,'%% Wing Apex coordinates. \n');
        for i=1:nw;
            fprintf(fid,'%6f %6f %6f \n', geo.startx(i),  geo.starty(i) , geo.startz(i));
        end
       

        fprintf(fid,'%%Wing root chords. \n');
        for i=1:nw;
            fprintf(fid,'%6f ', geo.c(i));
        end
            fprintf(fid,'\n');
        
        
        
        
        fprintf(fid,'%% Partition inner profiles. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', 0);
            end
            fprintf(fid,'\n');
        end
        
        
        fprintf(fid,'%% Partition outer profiles. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', 0);
            end
            fprintf(fid,'\n');
        end
        
        
        fprintf(fid,'%% Partition inner twist. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', geo.TW(i,j,1));
            end
            fprintf(fid,'\n');
        end
        
        
        fprintf(fid,'%% Partition outer twist. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', geo.TW(i,j,2));
            end
            fprintf(fid,'\n');
        end
        
        
        fprintf(fid,'%% Partition Dihedral. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', geo.dihed(i,j));
            end
            fprintf(fid,'\n');
        end
        
        
        fprintf(fid,'%% Partition Span. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', geo.b(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Taper. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', geo.T(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Sweep. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', geo.SW(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Chordwise panels. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ', geo.nx(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Spanwise panels. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ', geo.ny(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition distribution panels. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ', geo.meshtype(i,j));
            end
            fprintf(fid,'\n');
        end
        
        
        
        %Flaps
        fprintf(fid,'%% Partition Flap set bit. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ', geo.flapped(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Flap deflection symmetry set bit. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ', geo.fsym(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Flap chord. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', geo.fc(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Flap chordwise panels. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ', geo.fnx(i,j));
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Flap deflection. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', geo.flap_vector(i,j));
            end
            fprintf(fid,'\n');
        end
        
        
        
        
        %SLATS
        fprintf(fid,'%% Partition Slat set bit. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ', 0);
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Slat deflection symmetry set bit. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ', 0);
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Slat chord. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', 0);
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Slat chordwise panels. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%i ',0);
            end
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Partition Slat deflection. \n');
        for i=1:nw;
            for j=1:np
                fprintf(fid,'%6f ', 0);
            end
            fprintf(fid,'\n');
        end
        
        %ALLMOVE Definitions
        fprintf(fid,'%% Wing allmoving set bit. \n');
        for i=1:nw;
            fprintf(fid,'%i ', 0);
           
        end
        fprintf(fid,'\n');
        fprintf(fid,'%% Wing allmoving deflection symmetry set bit. \n');
        for i=1:nw;
            fprintf(fid,'%i ', 0); 
            
        end
        fprintf(fid,'\n');
        fprintf(fid,'%% Wing allmoving rotation origin. \n');
        for i=1:nw;
            fprintf(fid,'%6f %6f %6f ', 0, 0 , 0);
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%%  Wing allmoving rotation axis. \n');
        for i=1:nw;
            fprintf(fid,'%6f %6f %6f ', 0, 0 , 0);
            fprintf(fid,'\n');
        end
        
        fprintf(fid,'%% Wing allmoving deflection. \n');
        for i=1:nw;
            fprintf(fid,'%6f ', 0);
            fprintf(fid,'\n');
        end
        
   
        
        

        fprintf(fid,'\n');
        fprintf(fid,'%% End of file');
        q=fclose(fid);

        disp(' ')
        disp(' File written to:')
        disp(strcat(strcat(pwd,'\'),strcat(fname)));
        disp(' ')
     
end

