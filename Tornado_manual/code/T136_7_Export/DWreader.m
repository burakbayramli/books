function [dw, DW] = DWreader()

        fid = fopen('downwash.txt');
        A = fscanf(fid,'%f',[4,inf])';
        fclose(fid);
        
        side=sqrt(length(A));
        t=0;
        for i=1:side
            for j=1:side
                t=t+1;
            DW(i,j,:)=A(t,1:3);
            dw(i,j)=A(t,4);   
            end
        end
        

        
end

