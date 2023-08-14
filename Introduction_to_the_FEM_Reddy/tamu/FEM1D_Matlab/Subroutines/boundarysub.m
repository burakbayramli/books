function boundarysub(neq,nhbw,nspv,nssv,nnbc,ndf,dt,item,alfa,ibdy,ispv,issv,inbc,uref,vspv,vssv,vnbc,gu0,mxebc,mxnbc,mxmbc,mxneq,neqr)
%      __________________________________________________________________
%
%      the subroutine is called in main to implement specified boundary
%       conditions on the assembled system of finite element equations
%      __________________________________________________________________
%       implicit real*8 (a-h,o-z)
%       dimension  ispv(mxebc,2),issv(mxnbc,2),inbc(mxmbc,2),ibdy(mxebc)
%       dimension  uref(mxmbc),vspv(mxebc),vssv(mxnbc),vnbc(mxmbc)
%       dimension  glk(mxneq,mxneq),glm(mxneq,mxneq),glf(mxneq),gu0(mxneq)
%      __________________________________________________________________
% c     impose boundary conditions for static and time-dependent problems
%      __________________________________________________________________

global glk glf glm
if(item<=2)   %include specified primary degrees of freedom
    if(nspv~=0)
        for nb=1:nspv
            ie=(ispv(nb,1)-1)*ndf+ispv(nb,2);
            it=nhbw-1;
            i=ie-nhbw;
            for ii=1:it
                i=i+1;
                if(i>=1)
                    j=ie-i+1;
                    glf(i)=glf(i)-glk(i,j)*vspv(nb);
                    glk(i,j)=0.0;
                end
            end
            glk(ie,1)=1.0;
            glf(ie)=vspv(nb);
            i=ie;
            for ii=2:nhbw
                i=i+1;
                if (i<=neq)
                    glf(i)=glf(i)-glk(ie,ii)*vspv(nb);
                    glk(ie,ii)=0.0;
                end
            end
        end
    end
    if (nssv~=0) % c     include specified secondary degrees of freedom
        for nf=1:nssv
            nb=(issv(nf,1)-1)*ndf+issv(nf,2);
            if(item==1)
                glf(nb)=glf(nb)+vssv(nf)*dt;
            end
            if (item~=1)
                glf(nb)=glf(nb)+vssv(nf);
            end
        end
    end
    if (nnbc~=0) % c     include specified mixed boundary conditions
        for ic=1:nnbc 
            nc=(inbc(ic,1)-1)*ndf+inbc(ic,2);
            if(item==1)
                glk(nc,1)=glk(nc,1)+alfa*dt*vnbc(ic);
                glf(nc)=glf(nc)+dt*vnbc(ic)*(uref(ic)-(1.0-alfa)*gu0(nc));
            else
                glk(nc,1)=glk(nc,1)+vnbc(ic);
                glf(nc)=glf(nc)+vnbc(ic)*uref(ic);
            end
        end
    end
else
    % c     impose boundary conditions for eigenvalue problems
    if (nnbc~=0) % c     include specified mixed boundary conditions
        for ic=1:nnbc
            nc=(inbc(ic,1)-1)*ndf+inbc(ic,2);
            glk(nc,nc)=glk(nc,nc)+vnbc(ic);
        end
    end
    if (nspv~=0) % c     include specified primary degrees of freedom
        for ib=1:nspv
            ibdy(ib)=(ispv(ib,1)-1)*ndf+ispv(ib,2);
        end
        for i=1:nspv
            imax=ibdy(i);
            for j=i:nspv
                if (ibdy(j)>=imax)
                    imax=ibdy(j);
                    ikept=j;
                end
            end
            ibdy(ikept)=ibdy(i);
            ibdy(i)=imax;
        end
        neqr = neq;
        for i=1:nspv
            ib=ibdy(i);
            if (ib<neqr)
                neqr1=neqr-1;
                for ii=ib:neqr1
                    for jj=1:neqr
                        glm(ii,jj)=glm(ii+1,jj);
                        glk(ii,jj)=glk(ii+1,jj);
                    end
                    for jj=1:neqr
                        glm(jj,ii)=glm(jj,ii+1);
                        glk(jj,ii)=glk(jj,ii+1);
                    end
                end
            end
            neqr=neqr-1;
        end
    end
end
end
 
    

    


