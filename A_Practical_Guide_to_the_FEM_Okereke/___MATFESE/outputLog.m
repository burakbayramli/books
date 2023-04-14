%% CREATE OUTPUT/RESULTS DATA SCRIPT
%Author:    Dr. Michael I. Okereke
%Date:      18th July, 2016

%% Create Folder to save Output Files
    uu = dir('Job*');
    if length(uu) >= 1
        counter = length(uu)+1;
    elseif isempty(uu)
        counter = 1;
    end
        jobFolder   =   ['Job',num2str(counter),'_OutputFolder']                                                ; 
        mkdir(jobFolder);
        rezFile     =   ['Job',num2str(counter), '_Outputs.rez']                                                ;
        
%Load Data
load('memStiffnessData.mat')                                                                                    ;

%% Create Jobscript FFilename
    fidJScript  = fopen([jobFolder,'/',rezFile],'a+')                                                           ;
    fscriptName = fopen(fidJScript)                                                                             ;
   
    %Print the header content of filescript
    fprintf(fidJScript, '#------------------------------------------------------\n')                            ;
    fprintf(fidJScript, ['#   RESULTS FILE FOR JOB',num2str(counter),'         \n'])                                                            ;
    fprintf(fidJScript, '#---- >>  Author: User \n')                                                            ;
    fprintf(fidJScript, ['#---- >>  Date:   ',date,'\n'])                                                       ;
    fprintf(fidJScript, '#------------------------------------------------------\n')                            ;
    fprintf(fidJScript, '\n')                                                                                   ;

%% Write Geometry Outputs
    fprintf(fidJScript, '#---- >>  Material and Geometry Outputs \n')                                           ;
    fprintf(fidJScript, '#------------------------------------------------------\n')                            ;
    fprintf(fidJScript, ['Young''s Modulus,   E      = ', num2str(E/1e9),' GPa \n'])                             ;
    fprintf(fidJScript, ['Diameter of circle, d     = ', num2str(d),' m \n'])                                   ;
    fprintf(fidJScript, ['Scale Multiplier (Disp)   = ', num2str(scaleMultiplier), ' unit(s) \n'])              ;
    fprintf(fidJScript, '\n')                                                                                   ;
    fprintf(fidJScript, '#------------------------------------------------------\n')                            ;
        
%% STIFFNESS MATRICES
    fprintf(fidJScript, '#------------------------------------------------------\n')                            ;
    fprintf(fidJScript, ' >>>     STRIFFNESS MATRICES (MEMBER & GLOBAL)      <<<\n')                            ;
    fprintf(fidJScript, '-------------------------------------------------------\n')                            ;
    fprintf(fidJScript, '#Local Coordinates Member stiffness matrices \n')                             ;

    % Write Member Stiffness matrices to file
            k = cell(1);                
        for e = 1:numberElements
            %Calculate k
            for xs = 1:1
                %elementDof:    element degrees of freedom (Dof)
                indice          =   elementNodes(e,:);
                elementDof      =   [indice(1)*2-1 indice(1)*2 indice(2)*2-1 indice(2)*2];
                xa              =   xx(indice(2)) - xx(indice(1));
                ya              =   yy(indice(2)) - yy(indice(1));
                length_element  =   sqrt(xa*xa+ya*ya);
                k1              =   EA/length_element;
            end
            
           %% Now Print Member stiffness matrix to results output file
            fprintf(fidJScript, ['#For Member >> ',num2str(e),' \n'])                                                       ;
            fprintf(fidJScript, ['#---- >>  Local Member stiffness matrix, k',num2str(e), ' = ',num2str(memk1{e}),'*\n'])        	;
            fprintf(fidJScript,  '[ \n');
            dlmwrite([jobFolder,'/',rezFile],memStiffness{e}/memk1{e},'delimiter','\t', 'precision','%6.5g', '-append')          ;
            fprintf(fidJScript,  '] \n');
            fprintf(fidJScript, '\n')                                                                                       ;
           
           %% Now Print Member stiffness matrix to results output file
            fprintf(fidJScript, ['#Global Member Stiffness matrix, K',num2str(e), ' = ',num2str(memk1{e}),'*\n'])       	;
            fprintf(fidJScript,  '[ \n');
            dlmwrite([jobFolder,'/',rezFile],structStiffness{e}/memk1{e},'delimiter','\t', 'precision','%6.5g', '-append')       ;
            fprintf(fidJScript,  '] \n');
            fprintf(fidJScript, '\n')                                                                                       ;
        end
        
%% Write Structure Stiffness Matrix
            %Power of the stiffness matrix
            pIndex = round(log10(memk1{e}));
            fprintf(fidJScript, '#---- >>  Structure Stiffness matrices \n')                                    ;
            fprintf(fidJScript, '#------------------------------------------------------\n')                    ;
            fprintf(fidJScript, ['Structure Stiffness matrix, Ks = 10^',num2str(pIndex),'* \n'])             	;
            fprintf(fidJScript,  '[ \n');
            dlmwrite([jobFolder,'/',rezFile],stiffness/10^pIndex,'delimiter','\t', 'precision','%8.5g', '-append');
            fprintf(fidJScript,  '] \n');


%% EXTERNAL NODAL FORCES AND DISPLACEMENTS OUTPUTS
 %Display Results
    fprintf(fidJScript, '#------------------------------------------------------\n')                            ;
    fprintf(fidJScript, ' >>>   EXTERNAL FORCES AND DISPLACEMENT RESULTS     <<<\n')                            ;
    fprintf(fidJScript, '-------------------------------------------------------\n')                            ;
    fprintf(fidJScript, 'The nodal displacements are   >>> U =                   \n')                            ;
    fprintf(fidJScript,  '[ \n');
    dlmwrite([jobFolder,'/',rezFile],displacement,'delimiter','\t', 'precision','%8.5g', '-append')             ;
    fprintf(fidJScript,  '] \n');
    fprintf(fidJScript, '-------------------------------------------------------\n')                            ;
    fprintf(fidJScript, 'The nodal forces are          >>>  F =                 \n')                            ;
    fprintf(fidJScript,  '[ \n');
    dlmwrite([jobFolder,'/',rezFile],rezForce,'delimiter','\t', 'precision','%8.5g', '-append')                 ;
    fprintf(fidJScript,  '] \n');
    fprintf(fidJScript, '-------------------------------------------------------\n')                            ;
    

%% INTERNAL MEMBER DISPLACEMENTS, FORCES AND STRESSES
% Display Results
    fprintf(fidJScript, '#------------------------------------------------------\n')                            ;
    fprintf(fidJScript, ' >>>   INTERNAL PARAMETERS: FORCES AND STRESSES     <<<\n')                            ;
    fprintf(fidJScript, '-------------------------------------------------------\n')                            ;
    
    for e = 1:numberElements
%% Now Print Member/local displacement matrix to results output file
            for xs = 1:1
                %elementDof:    element degrees of freedom (Dof)
                indice          =   elementNodes(e,:);
                elementDof      =   [indice(1)*2-1 indice(1)*2 indice(2)*2-1 indice(2)*2];
                xa              =   xx(indice(2)) - xx(indice(1));
                ya              =   yy(indice(2)) - yy(indice(1));
                length_element  =   sqrt(xa*xa+ya*ya);
            end
            %Write to file member local displacements
            fprintf(fidJScript, ['#For Member >> ',num2str(e),' << node [',num2str(indice(1)),'-',num2str(indice(2)),'] \n'])        ;                                                       ;
            fprintf(fidJScript, ['Member Displacements, u',num2str(e),' = \n'])                                 ;
            fprintf(fidJScript,  '[ \n');
            dlmwrite([jobFolder,'/',rezFile],dispLocal{e},'delimiter','\t', 'precision','%6.5g', '-append')                         ;
            fprintf(fidJScript,  '] \n');
            fprintf(fidJScript, '\n')                                                                                               ;
            
            %Write to file internal member forces                                                       ;
            fprintf(fidJScript, ['Member Internal Force, F',num2str(e),' = ',num2str(memForces{e}), ' N \n'])                                 ;
            fprintf(fidJScript, '\n');
            
            %Write to file internal member stresses                                                    ;
            fprintf(fidJScript, ['Member Internal Stresses, Sigma',num2str(e),' = ',num2str(memStresses{e}/1e6), ' MPa \n'])                    ;
            fprintf(fidJScript, '\n')                                                                                               ;
            fprintf(fidJScript, '-------------------------------------------------------\n')                                        ;
            
    end

%% Copy model keyword file into Job folder
    copyfile(file.name, [jobFolder,'/'],'f');
        
 %% Close all open files
    fclose all;
        