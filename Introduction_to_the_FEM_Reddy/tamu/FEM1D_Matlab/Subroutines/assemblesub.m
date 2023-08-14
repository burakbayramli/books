function assemblesub(nod,mxelm,mxneq,ndf,npe,ne,item)
%      __________________________________________________________________
%
%        the subroutine is called in main to assemble element coefficient
%        matrices (in a upper banded matrix form) and right-hand vectors
%
%         {elf}.... element source vector, {f}
%         {elk}.... element coefficient matrix, [k]
%         {elm}.... element coefficient matrix, [m]
%         [nod].... connectivity matrix, [b]
%      __________________________________________________________________
 
%       implicit real*8 (a-h,o-z)
global glk glf glm elf elk elm
% elk=zeros(9,9); elm=zeros(9,9); elf=zeros(9,1);elx=zeros(4,1);elu=zeros(9,1);elv=zeros(9,1);ela=zeros(9,1);
if (item<=2)
    % assemble element coefficient matrix elk and source vector elf
    for i=1:npe                 %FULL MATRIX
        nr=(nod(ne,i)-1)*ndf;
        for ii=1:ndf
            nr=nr+1;
            l=(i-1)*ndf+ii;
            glf(nr)=glf(nr)+elf(l);
            for j=1:npe
                ncl=(nod(ne,j)-1)*ndf;
                for jj=1:ndf
                    m=(j-1)*ndf+jj;
                    nc=ncl-nr+jj+1;
                    if(nc>0)
                        glk(nr,nc)=glk(nr,nc)+elk(l,m);
                    end
                end
            end
        end
    end
else
    %     assemble element matrices into full global matrices
    for i=1:npe
        nr=(nod(ne,i)-1)*ndf;
        for ii=1:ndf
            nr=nr+1;
            l=(i-1)*ndf+ii;
            for j=1:npe
                nc=(nod(ne,j)-1)*ndf;
                for jj=1:ndf
                    m=(j-1)*ndf+jj;
                    nc=nc+1;
                    glk(nr,nc)=glk(nr,nc)+elk(l,m);
                    glm(nr,nc)=glm(nr,nc)+elm(l,m);
                end
            end
        end
    end
end
end
