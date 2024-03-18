function []=fExportDownwash(DW)

filename='downwash.txt'

[a b]=size(DW);
c=3;
%% Downwash matrix


STR=[];
for i=1:a
    
    STR=[STR '['];
    for j=1:b
        
        STR=[STR '['];
        STR=strcat(STR,num2str(DW(i,j)));
        STR=[STR ']'];
        if j<b
            STR=[STR ',' '\n'];
        end
    end
    STR=[STR ']' ];
    if i<a
        STR=[STR ',' '\n'];
    end
end


fid = fopen(filename,'w');
P=fprintf(fid,STR);
fclose(fid);


