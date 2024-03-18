function [op]=texport(JID,fname,quest) %***

settings=config('startup');
cd(settings.odir)

switch(quest) %***
    case(14) %***
        load(strcat(JID,'-Cx'))

        fid=fopen(strcat(fname,'.txt'),'w');

        fprintf(fid,'%%Tornado exported result data. \n');
        fprintf(fid,strcat(strcat('%%Date: ', date), '\n'));
        fprintf(fid,strcat(strcat('%%JID: ', JID), '\n'));
        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Solution condition: \n');
        fprintf(fid,'\n');

        fprintf(fid,'%4f', results.dwcond);
        fprintf(fid,('\t    %% Solver numerical condition. \n'));

        fprintf(fid,'\n');
        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%State: \n');
        fprintf(fid,'\n');


        fprintf(fid,'%4f', state.AS);
        fprintf(fid,('\t    %% Airspeed \n'));
        fprintf(fid,'%4f', state.alpha);
        fprintf(fid,('\t    %% Angle of attack, [rad] \n'));
        fprintf(fid,'%4f', state.betha);
        fprintf(fid,('\t    %% Angle of sideslip, [rad] \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f', state.P);
        fprintf(fid,('\t    %% Roll rate, [rad/s] \n'));
        fprintf(fid,'%6f', state.Q);
        fprintf(fid,('\t    %% Pitch rate, [rad/s] \n'));
        fprintf(fid,'%6f', state.R);
        fprintf(fid,('\t    %% Yaw rate, [rad/s] \n'));

        fprintf(fid,'\n')  ;

        fprintf(fid,'%6f', state.rho);
        fprintf(fid,('\t    %% Air density, [kg/m^3] \n'));
        fprintf(fid,'%6f', state.ALT);
        fprintf(fid,('\t    %% Altitude, [m] \n'));
        fprintf(fid,'%6f', state.pgcorr);
        fprintf(fid,('\t    %% Prandtl-Glauert correction enabled, [1/0] \n')); %***Pradtl changed to Prandtl (AT)
        fprintf(fid,'\n') ;


        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Reference units: \n');
        fprintf(fid,'\n');

        fprintf(fid,'%6f', ref.S_ref);
        fprintf(fid,('\t    %% Reference area, [m^2] \n'));
        fprintf(fid,'%6f', ref.C_mac);
        fprintf(fid,('\t    %% Mean Aero Chord, [m] \n'));
        fprintf(fid,'%6f', ref.b_ref);
        fprintf(fid,('\t    %% Reference span, [m] \n'));
        fprintf(fid,'\n');






        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Coefficients: \n');
        fprintf(fid,'\n');


        fprintf(fid,'%6f', results.CL);
        fprintf(fid,('\t    %% CL \n'));
        fprintf(fid,'%6f', results.CD);
        fprintf(fid,('\t    %% CD \n'));
        fprintf(fid,'%6f', results.CY);
        fprintf(fid,('\t    %% CY \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.Cl);
        fprintf(fid,('\t    %% Cl \n'));
        fprintf(fid,'%6f', results.Cm);
        fprintf(fid,('\t    %% Cm \n'));
        fprintf(fid,'%6f', results.Cn);
        fprintf(fid,('\t    %% Cn \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.CX);
        fprintf(fid,('\t    %% CX \n'));
        fprintf(fid,'%6f', results.CY);
        fprintf(fid,('\t    %% CY \n'));
        fprintf(fid,'%6f', results.CZ);
        fprintf(fid,('\t    %% CZ \n'));
        fprintf(fid,'\n');

        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Alpha derivatives: \n');
        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.CL_a);
        fprintf(fid,('\t    %% CL_a \n'));
        fprintf(fid,'%6f', results.CD_a);
        fprintf(fid,('\t    %% CD_a \n'));
        fprintf(fid,'%6f', results.CY_a);
        fprintf(fid,('\t    %% CY_a \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.Cl_a);
        fprintf(fid,('\t    %% Cl_a \n'));
        fprintf(fid,'%6f', results.Cm_a);
        fprintf(fid,('\t    %% Cm_a \n'));
        fprintf(fid,'%6f', results.Cn_a);
        fprintf(fid,('\t    %% Cn_a \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Beta derivatives: \n');
        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.CL_b);
        fprintf(fid,('\t    %% CL_b \n'));
        fprintf(fid,'%6f', results.CD_b);
        fprintf(fid,('\t    %% CD_b \n'));
        fprintf(fid,'%6f', results.CY_b);
        fprintf(fid,('\t    %% CY_b \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.Cl_b);
        fprintf(fid,('\t    %% Cl_b \n'));
        fprintf(fid,'%6f', results.Cm_b);
        fprintf(fid,('\t    %% Cm_b \n'));
        fprintf(fid,'%6f', results.Cn_b);
        fprintf(fid,('\t    %% Cn_b \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Roll rate derivatives: \n');
        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.CL_P);
        fprintf(fid,('\t    %% CL_P \n'));
        fprintf(fid,'%6f', results.CD_P);
        fprintf(fid,('\t    %% CD_P \n'));
        fprintf(fid,'%6f', results.CY_P);
        fprintf(fid,('\t    %% CY_P \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.Cl_P);
        fprintf(fid,('\t    %% Cl_P \n'));
        fprintf(fid,'%6f', results.Cm_P);
        fprintf(fid,('\t    %% Cm_P \n'));
        fprintf(fid,'%6f', results.Cn_P);
        fprintf(fid,('\t    %% Cn_P \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Pitch rate derivatives: \n');
        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.CL_P);
        fprintf(fid,('\t    %% CL_P \n'));
        fprintf(fid,'%6f', results.CD_P);
        fprintf(fid,('\t    %% CD_P \n'));
        fprintf(fid,'%6f', results.CY_P);
        fprintf(fid,('\t    %% CY_P \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.Cl_P);
        fprintf(fid,('\t    %% Cl_P \n'));
        fprintf(fid,'%6f', results.Cm_P);
        fprintf(fid,('\t    %% Cm_P \n'));
        fprintf(fid,'%6f', results.Cn_P);
        fprintf(fid,('\t    %% Cn_P \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Yaw rate derivatives: \n');
        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.CL_R);
        fprintf(fid,('\t    %% CL_R \n'));
        fprintf(fid,'%6f', results.CD_R);
        fprintf(fid,('\t    %% CD_R \n'));
        fprintf(fid,'%6f', results.CY_R);
        fprintf(fid,('\t    %% CY_R \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f', results.Cl_R);
        fprintf(fid,('\t    %% Cl_R \n'));
        fprintf(fid,'%6f', results.Cm_R);
        fprintf(fid,('\t    %% Cm_R \n'));
        fprintf(fid,'%6f', results.Cn_R);
        fprintf(fid,('\t    %% Cn_R \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Control surface deflection derivatives: \n');
        fprintf(fid,'%%CS   #1     #2     #3     ... \n');


        fprintf(fid,'\n');

        fprintf(fid,'%6f \t', results.CL_d);
        fprintf(fid,('\t    %% CL_d \n'));
        fprintf(fid,'%6f \t', results.CD_d);
        fprintf(fid,('\t    %% CD_d \n'));
        fprintf(fid,'%6f \t', results.CY_d);
        fprintf(fid,('\t    %% CY_d \n'));

        fprintf(fid,'\n');

        fprintf(fid,'%6f \t', results.Cl_d);
        fprintf(fid,('\t    %% Cl_d \n'));
        fprintf(fid,'%6f \t', results.Cm_d);
        fprintf(fid,('\t    %% Cm_d \n'));
        fprintf(fid,'%6f \t', results.Cn_d);
        fprintf(fid,('\t    %% Cn_d \n'));



        fprintf(fid,'%%--------------------------------------------- \n');
        fprintf(fid,'%%Spanload: \n');
        fprintf(fid,'%%Station 1 2 3 4...n \n');

        fprintf(fid,'\n');

        fprintf(fid,'%6f \t', full(results.ystation));
        fprintf(fid,('\t    %% Span station \n'));
        fprintf(fid,'%6f \t', full(results.ForcePerMeter));
        fprintf(fid,('\t    %% Force Per Meter [N/m] \n'));
        fprintf(fid,'%6f \t', full(results.CL_local));
        fprintf(fid,('\t    %% Spanwise local CL [-/m] \n'));
        fprintf(fid,'\n');
        fprintf(fid,'End of file');
        fclose(fid);

        disp(' ')
        disp(' File written to:')
        disp(strcat(strcat(pwd,'\'),strcat(fname,'.txt')));
        disp(' ')
        op=0;

        cd(settings.hdir)

        
        
    case(15) %***
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %***
        %function infofile() %***
        
        load(strcat(JID,'-Cx_drginf')) %***
        %Output all information from breakdown to text file %***
        fid=fopen(strcat(fname,'-drginf.txt'),'w'); %***
      
 
        % Manipulating drag results matrices for output %***

        %Initialising display variables %***
        MASTDRGdisp=zeros(numbmhs+1,numbcls+1);TCDTINCdisp=zeros(numbmhs+1,numbcls+1); %***
        CLHTAILdisp=zeros(numbmhs+1,numbcls+1);TCDWINCdisp=zeros(numbmhs+1,numbcls+1); %***
        TTTHRSTdisp=zeros(numbmhs+1,numbcls+1);TCDCINCdisp=zeros(numbmhs+1,numbcls+1); %***
        CLWINGAdisp=zeros(numbmhs+1,numbcls+1);dclwdcldisp=zeros(numbmhs+1,numbcls+1); %***
        TTLTRMDdisp=zeros(numbmhs+1,numbcls+1);CMALPHAdisp=zeros(numbmhs+1,numbcls+1); %***
        CDALPHAdisp=zeros(numbmhs+1,numbcls+1);DEALPHAdisp=zeros(numbmhs+1,numbcls+1); %***
        MASTDIZdisp=zeros(numbmhs+1,numbcls+1);

        fprintf(fid,'Drag and Associated Results from Polar Breakdown\n\n'); %***

        % Total Drag (MASTDRG) %***
        fprintf(fid,'---- Total Drag of Aircraft (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        MASTDRGdisp=MASTDRG; %***
        MASTDRGdisp(1,:)=MASTDRGdisp(1,:)/10; %***
        MASTDRGdisp(:,1)=MASTDRGdisp(:,1)/10; %***

        for n=1:1:(size(MASTDRGdisp,1)) %***
            fprintf(fid,'%10.2f',MASTDRGdisp(n,:)); %***
            fprintf(fid,'\n'); %***
        end %***

        % Auto-Vortex Induced Drag of Wing (vorinrd) %***
        fprintf(fid,'\n\n---- Auto-Vortex Induced Drag of Main Wing (Counts) ----\n'); %***
        fprintf(fid,'            / Operating Lift Coefficient\n\n'); %***
        fprintf(fid,'%10.2f',(MASTDRG(1,:)/10)); %***
        fprintf(fid,'\n'); %***
        fprintf(fid,'%10.2f',vorinrd'); %***
        fprintf(fid,'\n'); %***

        % Auto-Vortex Induced Drag Factor of Wing (VORINDF) %***
        fprintf(fid,'\n\n---- Wing Auto-Induced Drag Factor and Corresponding CL ----\n\n'); %***
        fprintf(fid,'   Drag Factor      CL  \n');
        fprintf(fid,'%12.4f',VORINDF); %***
        fprintf(fid,'%12.4f',VOINFCL); %***
        fprintf(fid,'\n'); %***

        % Equivalent Oswald Factor (OSWALDF) %***
        fprintf(fid,'\n\n---- Equivalent Oswald Factor ----\n\n'); %***
        fprintf(fid,'%12.4f',OSWALDF); %***
        fprintf(fid,'\n'); %***

        % Zero-Lift Drag Coefficient (EZELDRG) %***
        fprintf(fid,'\n\n---- Zero-Lift Drag (Counts) ----\n\n'); %***
        fprintf(fid,'%12.2f',EZELDRG); %***
        fprintf(fid,'\n'); %***

        % Form Drag as a Function of Operating Lift Coefficient (PROFCOR) %***
        fprintf(fid,'\n\n---- Form Drag as a Function of Operating Lift Coefficient (Counts) ----\n'); %***
        fprintf(fid,'            / Operating Lift Coefficient\n\n'); %***
        fprintf(fid,'%11.2f',(MASTDRG(1,:)/10)); %***
        fprintf(fid,'\n'); %***
        fprintf(fid,'%11.3f',PROFCOR); %***
        fprintf(fid,'\n'); %***

        % Trim Drag due to H-Tail Vortex-Induced Drag (TCDTINC) %***
        fprintf(fid,'\n\n---- Trim Drag due to H-Tail Vortex-Induced Drag (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TCDTINCdisp=TCDTINC; %***
        TCDTINCdisp(1,:)=MASTDRGdisp(1,:); %***
        TCDTINCdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TCDTINCdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',TCDTINCdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',TCDTINCdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Lift Coefficient of H-Tail (CLHTAIL) %***
        fprintf(fid,'\n\n---- Lift Coefficient of H-Tail ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        CLHTAILdisp=CLHTAIL; %***
        CLHTAILdisp(1,:)=MASTDRGdisp(1,:); %***
        CLHTAILdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(CLHTAILdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',CLHTAILdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',CLHTAILdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Trim Drag due to Wing Incremental Vortex-Induced Drag (TCDWINC) %***
        fprintf(fid,'\n\n---- Trim Drag due to Wing Incremental Vortex-Induced Drag (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TCDWINCdisp=MASTDRG; %***
        TCDWINCdisp(1,:)=MASTDRGdisp(1,:); %***
        TCDWINCdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TCDWINCdisp,1)) %***
            fprintf(fid,'%10.2f',TCDWINCdisp(n,:)); %***
            fprintf(fid,'\n'); %***
        end %***

        % Tail Thrust (Negative Drag) due to Wing Downwash (TTTHRST) %***
        fprintf(fid,'\n\n---- Tail Thrust (Negative Drag) due to Wing Downwash (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TTTHRSTdisp=TTTHRST; %***
        TTTHRSTdisp(1,:)=MASTDRGdisp(1,:); %***
        TTTHRSTdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TTTHRSTdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',TTTHRSTdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',TTTHRSTdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Incremental Compressibility Drag due to Wing Lift Increment (TCDCINC) %***
        fprintf(fid,'\n\n---- Incremental Compressibility Drag due to Wing Lift Increment (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TCDCINCdisp=TCDCINC; %***
        TCDCINCdisp(1,:)=MASTDRGdisp(1,:); %***
        TCDCINCdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TCDCINCdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',TCDCINCdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',TCDCINCdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Actual Lift Coefficient of Main Wing (CLWINGA) %***
        fprintf(fid,'\n\n---- Actual Lift Coefficient of Main Wing ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        CLWINGAdisp=CLWINGA; %***
        CLWINGAdisp(1,:)=MASTDRGdisp(1,:); %***
        CLWINGAdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(CLWINGAdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',CLWINGAdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',CLWINGAdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Rate Change of Wing Lift w.r.t. Global Lift Coefficient (dclwdcl) %***
        fprintf(fid,'\n\n---- Rate Change of Wing Lift w.r.t. Global Lift Coefficient ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        dclwdcldisp=dclwdcl; %***
        dclwdcldisp(1,:)=MASTDRGdisp(1,:); %***
        dclwdcldisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(dclwdcldisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',dclwdcldisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',dclwdcldisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Total Trim Drag (TTLTRMD) %***
        fprintf(fid,'\n\n---- Total Trim Drag of Aircraft (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TTLTRMDdisp=TTLTRMD; %***
        TTLTRMDdisp(1,:)=MASTDRGdisp(1,:); %***
        TTLTRMDdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TTLTRMDdisp,1)) %***
            fprintf(fid,'%10.2f',TTLTRMDdisp(n,:)); %***
            fprintf(fid,'\n'); %***
        end %***

        % Pitching Moment Gradient due to AoA (CMALPHA) %***
        fprintf(fid,'\n\n---- Pitching Moment Gradient due to Angle of Attack (***Units?) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        CMALPHAdisp=CMALPHA; %***
        CMALPHAdisp(1,:)=MASTDRGdisp(1,:); %***
        CMALPHAdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(CMALPHAdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',CMALPHAdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',CMALPHAdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Rate Change of Total Drag w.r.t. AoA (CDALPHA) %***
        fprintf(fid,'\n\n---- Rate Change of Total Drag w.r.t. Angle of Attack (***Units?)----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        CDALPHAdisp=CDALPHA; %***
        CDALPHAdisp(1,:)=MASTDRGdisp(1,:); %***
        CDALPHAdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(CDALPHAdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',CDALPHAdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',CDALPHAdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Difference Between Given AoA and Zero-Lift AoA (DEALPHA) %***
        fprintf(fid,'\n\n---- Difference Between Given AoA and Zero-Lift AoA (rad)----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        DEALPHAdisp=DEALPHA; %***
        DEALPHAdisp(1,:)=MASTDRGdisp(1,:); %***
        DEALPHAdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(DEALPHAdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',DEALPHAdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',DEALPHAdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Compressibility Drag Matrix Adjusted for Wing Incremental Lift Effect (MASTDIZ) %***
        fprintf(fid,'\n\n---- Compressibility Drag Matrix Adjusted for Wing Incremental Lift Effect (Counts)----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        MASTDIZdisp=MASTDIZ; %***
        MASTDIZdisp(1,:)=MASTDRGdisp(1,:); %***
        MASTDIZdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(MASTDIZdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',MASTDIZdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',MASTDIZdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***


        fprintf(fid,'\n\n*** End of File ***'); %***
        fclose(fid); %***

        disp(' ') %***
        disp(' File written to:') %***
        disp(strcat(strcat(pwd,'\'),strcat(fname,'-drginf.txt'))); %***
        op=0; %***

        cd(settings.hdir) %***        
        
        %end %infofile function %***
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %function drgfile() %***
        cd(settings.odir)
        load(strcat(JID,'-Cx_drgout')) %***
        
        %Output drag information from breakdown to text file %***
        fid=fopen(strcat(fname,'-drgout.txt'),'w'); %***
 
        % Manipulating drag results matrices for output %***
        %otptcls=((MASTCLS(MASTCLS~=0))/10)'; %***
        %otptmch=(MASTMCH/10)'; %***

        %Initialising display variables %***
        MASTDRGdisp=zeros(numbmhs+1,numbcls+1);TCDTINCdisp=zeros(numbmhs+1,numbcls+1); %***
        TCDWINCdisp=zeros(numbmhs+1,numbcls+1);TTTHRSTdisp=zeros(numbmhs+1,numbcls+1); %***
        TCDCINCdisp=zeros(numbmhs+1,numbcls+1);TTLTRMDdisp=zeros(numbmhs+1,numbcls+1); %***
        CDALPHAdisp=zeros(numbmhs+1,numbcls+1);MASTDIZdisp=zeros(numbmhs+1,numbcls+1); %***

        fprintf(fid,'Constituent Components of Drag from Polar Breakdown\n\n'); %***

        % Total Drag (MASTDRG) %***
        fprintf(fid,'---- Total Drag of Aircraft (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        MASTDRGdisp=MASTDRG; %***
        MASTDRGdisp(1,:)=MASTDRGdisp(1,:)/10; %***
        MASTDRGdisp(:,1)=MASTDRGdisp(:,1)/10; %***

        for n=1:1:(size(MASTDRGdisp,1)) %***
            fprintf(fid,'%10.2f',MASTDRGdisp(n,:)); %***
            fprintf(fid,'\n'); %***
        end %***

        % Auto-Vortex Induced Drag of Wing (vorinrd) %***
        fprintf(fid,'\n\n---- Auto-Vortex Induced Drag of Main Wing (Counts) ----\n'); %***
        fprintf(fid,'            / Operating Lift Coefficient\n\n'); %***
        fprintf(fid,'%10.2f',(MASTDRG(1,:)/10)); %***
        fprintf(fid,'\n'); %***
        fprintf(fid,'%10.2f',vorinrd'); %***
        fprintf(fid,'\n'); %***

        % Auto-Vortex Induced Drag Factor of Wing (VORINDF) %***
        fprintf(fid,'\n\n---- Wing Auto-Induced Drag Factor and Corresponding CL ----\n\n'); %***
        fprintf(fid,'   Drag Factor      CL  \n');
        fprintf(fid,'%12.4f',VORINDF); %***
        fprintf(fid,'%12.4f',VOINFCL); %***
        fprintf(fid,'\n'); %***

        % Zero-Lift Drag Coefficient (EZELDRG) %***
        fprintf(fid,'\n\n---- Zero-Lift Drag (Counts) ----\n\n'); %***
        fprintf(fid,'%12.2f',EZELDRG); %***
        fprintf(fid,'\n'); %***

        % Form Drag as a Function of Operating Lift Coefficient (PROFCOR) %***
        fprintf(fid,'\n\n---- Form Drag as a Function of Operating Lift Coefficient (Counts) ----\n'); %***
        fprintf(fid,'            / Operating Lift Coefficient\n\n'); %***
        fprintf(fid,'%11.2f',(MASTDRG(1,:)/10)); %***
        fprintf(fid,'\n'); %***
        fprintf(fid,'%11.3f',PROFCOR); %***
        fprintf(fid,'\n'); %***

        % Trim Drag due to H-Tail Vortex-Induced Drag (TCDTINC) %***
        fprintf(fid,'\n\n---- Trim Drag due to H-Tail Vortex-Induced Drag (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TCDTINCdisp=TCDTINC; %***
        TCDTINCdisp(1,:)=MASTDRGdisp(1,:); %***
        TCDTINCdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TCDTINCdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',TCDTINCdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',TCDTINCdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Trim Drag due to Wing Incremental Vortex-Induced Drag (TCDWINC) %***
        fprintf(fid,'\n\n---- Trim Drag due to Wing Incremental Vortex-Induced Drag (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TCDWINCdisp=MASTDRG; %***
        TCDWINCdisp(1,:)=MASTDRGdisp(1,:); %***
        TCDWINCdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TCDWINCdisp,1)) %***
            fprintf(fid,'%10.2f',TCDWINCdisp(n,:)); %***
            fprintf(fid,'\n'); %***
        end %***

        % Tail Thrust (Negative Drag) due to Wing Downwash (TTTHRST) %***
        fprintf(fid,'\n\n---- Tail Thrust (Negative Drag) due to Wing Downwash (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TTTHRSTdisp=TTTHRST; %***
        TTTHRSTdisp(1,:)=MASTDRGdisp(1,:); %***
        TTTHRSTdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TTTHRSTdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',TTTHRSTdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',TTTHRSTdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Incremental Compressibility Drag due to Wing Lift Increment (TCDCINC) %***
        fprintf(fid,'\n\n---- Incremental Compressibility Drag due to Wing Lift Increment (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TCDCINCdisp=TCDCINC; %***
        TCDCINCdisp(1,:)=MASTDRGdisp(1,:); %***
        TCDCINCdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TCDCINCdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',TCDCINCdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',TCDCINCdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Total Trim Drag (TTLTRMD) %***
        fprintf(fid,'\n\n---- Total Trim Drag of Aircraft (Counts) ----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        TTLTRMDdisp=TTLTRMD; %***
        TTLTRMDdisp(1,:)=MASTDRGdisp(1,:); %***
        TTLTRMDdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(TTLTRMDdisp,1)) %***
            fprintf(fid,'%10.2f',TTLTRMDdisp(n,:)); %***
            fprintf(fid,'\n'); %***
        end %***

        % Rate Change of Total Drag w.r.t. AoA (CDALPHA) %***
        fprintf(fid,'\n\n---- Rate Change of Total Drag w.r.t. Angle of Attack (***Units?)----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        CDALPHAdisp=CDALPHA; %***
        CDALPHAdisp(1,:)=MASTDRGdisp(1,:); %***
        CDALPHAdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(CDALPHAdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',CDALPHAdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',CDALPHAdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***

        % Compressibility Drag Matrix Adjusted for Wing Incremental Lift Effect (MASTDIZ) %***
        fprintf(fid,'\n\n---- Compressibility Drag Matrix Adjusted for Wing Incremental Lift Effect (Counts)----\n'); %***
        fprintf(fid,'Mach Number / Operating Lift Coefficient\n\n'); %***

        MASTDIZdisp=MASTDIZ; %***
        MASTDIZdisp(1,:)=MASTDRGdisp(1,:); %***
        MASTDIZdisp(:,1)=MASTDRGdisp(:,1); %***

        for n=1:1:(size(MASTDIZdisp,1)) %***
            if n==1 %***
                fprintf(fid,'%12.2f',MASTDIZdisp(n,:)); %***
            else %***
                fprintf(fid,'%12.4f',MASTDIZdisp(n,:)); %***
            end %***
            fprintf(fid,'\n'); %***
        end %***


        fprintf(fid,'\n\n*** End of File ***'); %***
        fclose(fid); %***

        disp(' ') %***
        disp(' File written to:') %***
        disp(strcat(strcat(pwd,'\'),strcat(fname,'-drgout.txt'))); %***
        disp(' ') %***
        op=0; %***

        cd(settings.hdir) %***

        %open 'dragout.txt' %***

        %end %drgfile function %***
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    otherwise %***
        return %***
        
end %***


end %texport function %***








