function eqnsolvr(neqns,nbw,band,rhs,ires)
% c     _________________________________________________________________
% c
% c     the subroutine is called in main to solve symmetric and banded set
% c     of equations using the gauss elimination method:[band]{u} = {rhs}.
% c     the coefficient matrix is input as band(neqns,nbw) and the column
% c     vector is input as  rhs(neqns),  where neqns is the actual number
% c     of equations and nbw is the half band width.  the true dimensions
% c     of the matrix [band] in the calling program, are nrm by ncm. when
% c     ires is greater than zero, the right hand elimination is skipped.
% c     _________________________________________________________________
% c
%       implicit real*8(a-h,o-z)
%       dimension band(nrm,ncm),rhs(nrm)
% band=zeros(nbw*10,neqns*10);
% rhs=zeros(neqns);
global pv_solution
meqns=neqns-1;
if(ires<=0)
    for npiv=1:meqns
        npivot=npiv+1;
        lstsub=npiv+nbw-1;
        if(lstsub>neqns)
            lstsub=neqns;
        end
        for nrow=npivot:lstsub
            ncol=nrow-npiv+1;
            factor=band(npiv,ncol)/band(npiv,1);
            for ncol=nrow:lstsub
                icol=ncol-nrow+1;
                jcol=ncol-npiv+1;
                band(nrow,icol)=(band(nrow,icol)-factor*band(npiv,jcol));
            end
            rhs(nrow)=rhs(nrow)-factor*rhs(npiv);
        end
    end
else
    for npiv=1:meqns
        npivot=npiv+1;
        lstsub=npiv+nbw-1;
        if(lstsub>neqns)
            lstsub=neqns;
        end
        for nrow=npivot:lstsub
            ncol=nrow-npiv+1;
            factor=band(npiv,ncol)/band(npiv,1);
            rhs(nrow)=rhs(nrow)-factor*rhs(npiv);
        end
    end
end
%c     back substitution

for ijk=2:neqns
    npiv=neqns-ijk+2;
    rhs(npiv)=rhs(npiv)/band(npiv,1);
    lstsub=npiv-nbw+1;
    if(lstsub<1)
        lstsub=1;
    end
    npivot=npiv-1;
    for jki=lstsub:npivot
        nrow=npivot-jki+lstsub;
        ncol=npiv-nrow+1;
        factor=band(nrow,ncol);
        rhs(nrow)=rhs(nrow)-factor*rhs(npiv);
    end
end
pv_solution(1)=rhs(1,1)/band(1,1);
for i=2:neqns
    pv_solution(i)=rhs(i,1);
end
end
