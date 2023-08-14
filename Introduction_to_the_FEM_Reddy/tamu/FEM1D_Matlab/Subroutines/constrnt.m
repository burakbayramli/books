function constrnt(neq,nhbw,ndf,ncon,icon,vcon,trm,mxneq)
% c     ____________________________________________________________________
% c
% c     the subroutine is called in main to implement specified constraint
% c      conditions (e.g., inclined supports) on the condensed system of
% c      equations. array glm is used here as a temporary storage array.
% c     ____________________________________________________________________
% c
% implicit real*8 (a-h,o-z)
% dimension  icon(9),vcon(9),glk(mxneq,mxneq),glf(mxneq),glm(mxneq,mxneq),trm(mxneq,mxneq)
%c
global glk glm glf
pi=3.14159265d0;

%c     include specified constraint conditions
for ic=1:neq
    for jc=1:neq
        glm(ic,jc)=0.0;
        trm(ic,jc)=0.0;
    end
    trm(ic,ic)=1.0d0;
end
for ic=1:ncon
    beta=vcon(ic)*pi/180.0d0;
    iforf=ndf*icon(ic)-1;
    trm(iforf,iforf)    = cos(beta);
    trm(iforf,iforf+1)  = sin(beta);
    trm(iforf+1:iforf)  =-sin(beta);
    trm(iforf+1:iforf+1)= cos(beta);
end

l=0;
for i=1:neq
    for j=1:nhbw
        glm(i,l+j)=glk(i,j);
    end
    l=l+1;
end
for i=1:neq
    for j=i:neq
        glm(j,i)=glm(i,j);
    end
end

for i=1:neq
    for j=1:neq
        glk(i,j)=glm(i,j);
    end
end

for i=1:neq
    for j=1:neq
        glm(i,j)=0.0;
        for k=1:neq
            glm(i,j)=glm(i,j)+trm(i,k)*glk(k,j);
        end
    end
end

for i=1:neq
    for j=1:neq
        glk(i,j)=0.0;
        for k=1:neq
            glk(i,j)=glk(i,j)+glm(i,k)*trm(j,k);
        end
    end
end


for i=1:neq
    for j=1:neq
        trm(i,j)=glk(i,j);
    end
end

l=0;
for i=1:neq
    for j=1:nhbw
        glk(i,j)=trm(i,l+j);
    end
    l=l+1;
end

for i=1:neq
    glm(i,1)=0.0;
    for k=1:neq
        glm(i,1)=glm(i,1)+trm(i,k)*glf(k);
    end
    glf(i)=glm(i,1);
end

return
end


