function [ output_args ] = fExportState(state)
%This function exports the state struct to text file for reading of the
%compiled C version of Tornado.
%   


 fname=('state.txt');
        fid=fopen(fname,'w');
        
        
        
        fprintf(fid,'%%Tornado exported state data. \n');
        fprintf(fid,strcat(strcat('%%Date: ', date), '\n'));
        fprintf(fid,'%%--------------------------------------------- \n');
        
        
            fprintf(fid,'%6f \n',state.AS);
            fprintf(fid,'%6f \n',state.alpha);
            fprintf(fid,'%6f \n',state.betha);
            fprintf(fid,'%6f \n',state.P);
            fprintf(fid,'%6f \n',state.Q);
            fprintf(fid,'%6f \n',state.R);
            fprintf(fid,'%6f \n',state.rho);
            
            
                    fprintf(fid,'\n');
        fprintf(fid,'%% End of file');
        q=fclose(fid);

        disp(' ')
        disp(' File written to:')
        disp(strcat(strcat(pwd,'\'),strcat(fname)));
        disp(' ')
        

end

