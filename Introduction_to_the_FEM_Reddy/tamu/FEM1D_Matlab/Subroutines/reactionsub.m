function reactionsub(mxelm,mxneq,ndf,nem,nod,npe,ntype,pr,glf,se,sl,sa,si,cs,sn,cnt,snt,hf,vf,pf,xb)
% c     __________________________________________________________________
% c
% c      the subroutine is called in main to compute generalized reaction
% c     forces in each element of truss (ndf=2) or frame (ndf=3) structure
% c     __________________________________________________________________
% c
%       implicit real*8(a-h,o-z)
%       dimension  pr(mxelm),se(mxelm),sl(mxelm),sa(mxelm),si(mxelm)
%       dimension  cs(mxelm),sn(mxelm),cnt(mxelm),snt(mxelm)
%       dimension  hf(mxelm),vf(mxelm),pf(mxelm),xb(mxelm)
%       dimension  nod(mxelm,4),glf(mxneq),elr(6)
%       common/stf1/elk(9,9),elm(9,9),elf(9),elx(4),elu(9),elv(9),ela(9)


nn=npe*ndf;
for n=1:nem
    cn1=cs(n);
    sn1=sn(n);
    % c     call transfrm to compute element stiffness matrix and force vector
    l=0;
    for i=1:npe
        ni=nod(n,i);
        li=(ni-1)*ndf;
        for j=1:ndf
            li=li+1;
            l=l+1;
            elu(l)=glf(li);
        end
    end
    [elk,elf]=transfrm(mxelm,n,ntype,pr,se,sl,sa,si,cs,sn,cnt,snt,hf,vf,pf,xb);
    % c     compute the force and moment resultants
    for i=1:nn
        elr(i) = 0.0;
        for j=1:nn
            elr(i) =  elr(i) + elk(i,j)*elu(j);
        end
        elr(i) =  elr(i) - elf(i);
    end
    elf(1) =  elr(1)*cn1+elr(2)*sn1;
    elf(2) = -elr(1)*sn1+elr(2)*cn1;
    if(ntype~=0)
        elf(3) =  elr(3);
        elf(4) =  elr(4)*cn1+elr(5)*sn1;
        elf(5) = -elr(4)*sn1+elr(5)*cn1;
        elf(6) =  elr(6);
    else
        elf(3) =  elr(3)*cn1+elr(4)*sn1;
        elf(4) = -elr(3)*sn1+elr(4)*cn1;
    end
    %     write(6,150)n, (elf(i),i=1,nn)
    %     write(6,160)   (elr(i),i=1,nn)
end
% 150 format (3x,i2,6e12.4)
% 160 format (5x,6e12.4,/)
end


