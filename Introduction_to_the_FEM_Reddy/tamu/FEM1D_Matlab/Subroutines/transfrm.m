function transfrm(mxelm,n,ntype,pr,se,sl,sa,si,cs,sn,cnt,snt,hf,vf,pf,xb)
% elm,elx,elu,elv,ela
% c     __________________________________________________________________
% c
% c     called in both main and reaction to compute stiffness matrix and
% c      force vector for the truss (ndf=2) and frame (ndf=3) elements
% c
% c     se......young's modulus
% c     sl......element length
% c     sa......cross-sectional area
% c     si......moment of inertia
% c     cs......cosine of the angle of orientation
% c     sn......sine of the angle of orientation
% c     hf......distributed force along the length of the element
% c     vf......distributed force transverse to the element
% c     pf......point force at point other than nodes
% c     xb......distance along the length from node 1 of the element
% c             of the location of the point force, pf
% c     cnt,snt:direction cosines of the point force's line of application
% c     __________________________________________________________________
% c
% implicit real*8(a-h,o-z)
% pr=zeros(mxelm,1);se=zeros(mxelm,1);sl=zeros(mxelm,1);sa=zeros(mxelm,1);si=zeros(mxelm,1);
% cs=zeros(mxelm,1);sn=zeros(mxelm,1);cnt=zeros(mxelm,1);snt=zeros(mxelm,1);
% hf=zeros(mxelm,1);vf=zeros(mxelm,1);pf=zeros(mxelm,1);xb=zeros(mxelm,1);
% common/stf1/elk(9,9),elm(9,9),elf(9),elx(4),elu(9),elv(9),ela(9)

global elk elf
trm=zeros(6,6);tmpk=zeros(6,6);
cn1=cs(n);
sn1=sn(n);
cn2=cn1*cn1;
sn2=sn1*sn1;
csn=cn1*sn1;
%c     element coefficients
if(ntype==0)
    %c     the plane truss element
    nn=4;
    c1=sa(n)*se(n)/sl(n);
    elk(1,1) = c1*cn2;
    elk(2,1) = c1*csn;
    elk(2,2) = c1*sn2;
    elk(3,1) = -elk(1,1);
    elk(3,2) = -elk(2,1);
    elk(3,3) =  elk(1,1);
    elk(4,1) = -elk(2,1);
    elk(4,2) = -elk(2,2);
    elk(4,3) = -elk(3,2);
    elk(4,4) =  elk(2,2);
    
    for i=1:nn
        for j=i:nn
            elk(i,j) = elk(j,i);
        end
    end
    %c     contribution of the point force to nodal forces
    xi=xb(n)/sl(n);
    sfl1 = 1.0-xi;
    sfl2 = xi;
    
    f1=0.5*hf(n)*sl(n);
    f3=0.5*hf(n)*sl(n);
    elf(1) = f1*cn1;
    elf(2) = f1*sn1;
    elf(3) = f3*cn1;
    elf(4) = f3*sn1;
else
    nn=6;
    if(ntype==1)
        %c     the euler-bernoulli frame element
        amu=0.5*sa(n)*sl(n)*sl(n)/si(n);
        c1=2.0*se(n)*si(n)/(sl(n)^3);
        c2=6.0*se(n)*si(n)/(sl(n)*sl(n));
        c3=c1*(amu*cn2+6.0*sn2);
        c4=c1*(amu-6.0)*csn;
        c5=c1*(amu*sn2+6.0*cn2);
        c6=4.0*se(n)*si(n)/sl(n);
        
        elk(1,1) = c3;
        elk(2,1) = c4;
        elk(2,2) = c5;
        elk(3,1) = c2*sn1;
        elk(3,2) =-c2*cn1;
        elk(3,3) = c6;
        elk(4,1) =-c3;
        elk(4,2) =-c4;
        elk(4,3) =-c2*sn1;
        elk(4,4) = c3;
        elk(5,1) =-c4;
        elk(5,2) =-c5;
        elk(5,3) = c2*cn1;
        elk(5,4) = c4;
        elk(5,5) = c5;
        elk(6,1) = c2*sn1;
        elk(6,2) =-c2*cn1;
        elk(6,3) = 0.5*c6;
        elk(6,4) =-c2*sn1;
        elk(6,5) = c2*cn1;
        elk(6,6) = c6;
        for i=1:nn
            for j=i:nn
                elk(i,j) = elk(j,i);
            end
        end
        %c     contribution of the point force to nodal generalized forces
        xi=xb(n)/sl(n);
        tf=pf(n)*snt(n);
        af=pf(n)*cnt(n);
        sfl1 = 1.0-xi;
        sfl2 = xi;
        sfh1 = 1.0 - 3.0*xi*xi + 2.0*(xi^3);
        sfh2 = -xi*(1.0+xi*xi-2.0*xi)*sl(n);
        sfh3 = 3.0*xi*xi - 2.0*(xi^3);
        sfh4 = -xi*(xi*xi - xi)*sl(n);
        
        f1=0.5*hf(n)*sl(n)         + sfl1*af;
        f2=0.5*vf(n)*sl(n)         + sfh1*tf;
        f3=-vf(n)*sl(n)*sl(n)/12.0 + sfh2*tf;
        f4=0.5*hf(n)*sl(n)         + sfl2*af;
        f5=0.5*vf(n)*sl(n)         + sfh3*tf;
        f6=vf(n)*sl(n)*sl(n)/12.0  + sfh4*tf;
        elf(1) = f1*cn1-f2*sn1;
        elf(2) = f1*sn1+f2*cn1;
        elf(3) = f3;
        elf(4) = f4*cn1-f5*sn1;
        elf(5) = f4*sn1+f5*cn1;
        elf(6) = f6;
    else
        %c     the timoshenko frame element (shear coefficient=5/6)
        
        sg=5.0*se(n)/(1.0+pr(n))/12.0;
        c1=sa(n)*se(n)/sl(n);
        c2=sg*sa(n)/sl(n);
        c3=0.5*sg*sa(n);
        c4=0.25*sg*sa(n)*sl(n);
        c5=se(n)*si(n)/sl(n);
        elk(1,1)=c1;
        elk(2,1)=0.0;
        elk(2,2)=c2;
        elk(3,1)=0.0;
        elk(3,2)=-c3;
        elk(3,3)=c4+c5;
        elk(4,1)=-c1;
        elk(4,2)=0.0;
        elk(4,3)=0.0;
        elk(4,4)=c1;
        elk(5,1)=0.0;
        elk(5,2)=-c2;
        elk(5,3)=c3;
        elk(5,4)=0.0;
        elk(5,5)=c2;
        elk(6,1)=0.0;
        elk(6,2)=-c3;
        elk(6,3)=c4-c5;
        elk(6,4)=0.0;
        elk(6,5)=c3;
        elk(6,6)=c4+c5;
        
        for i=1:nn
            for j=1:nn
                trm(j,i)=0.0;
            end
        end
        
        trm(1,1)=cn1;
        trm(1,2)=sn1;
        trm(2,1)=-sn1;
        trm(2,2)=cn1;
        trm(3,3)=1.0;
        trm(4,4)=cn1;
        trm(4,5)=sn1;
        trm(5,4)=-sn1;
        trm(5,5)=cn1;
        trm(6,6)=1.0;
        
        for i=1:nn
            for j=i:nn
                elk(i,j) = elk(j,i);
            end
        end

        for i=1:nn
            for j=1:nn
                tmpk(i,j)=0.0;
                for k=1:nn
                    tmpk(i,j)=tmpk(i,j)+trm(k,i)*elk(k,j);
                end
            end
        end
        for i=1:nn
            for j=1:nn
                elk(i,j)=0.0;
                for k=1:nn
                    elk(i,j)=elk(i,j)+tmpk(i,k)*trm(k,j);
                end
            end
        end
%         for i=1:nn
%             for j=1:nn
%                 fprintf('%d, %d, % 3.3f\n',i,j, trm(i,j));
%             end
%         end        
        %c     contribution of the point force to nodal generalized forces
        
        xi=xb(n)/sl(n);
        tf=pf(n)*snt(n);
        af=pf(n)*cnt(n);
        sfl1 = 1.0-xi;
        sfl2 = xi;
        sfq1 = (1.0-xi)*(1.0-2.0*xi);
        sfq2 = -xi*(1.0-2.0*xi);
        sfq3 = 4.0*xi*(1.0-xi);
        
        f1=0.5*hf(n)*sl(n)         + sfl1*af;
        f2=0.5*vf(n)*sl(n)         + (sfq1+0.5*sfq3)*tf;
        f3=-vf(n)*sl(n)*sl(n)/12.0 - 0.125*sfq3*sl(n)*tf;
        f4=0.5*hf(n)*sl(n)         + sfl2*af;
        f5=0.5*vf(n)*sl(n)         + (sfq2+0.5*sfq3)*tf;
        f6=vf(n)*sl(n)*sl(n)/12.0  + 0.125*sfq3*sl(n)*tf;
        elf(1) = f1*cn1-f2*sn1;
        elf(2) = f1*sn1+f2*cn1;
        elf(3) = f3;
        elf(4) = f4*cn1-f5*sn1;
        elf(5) = f4*sn1+f5*cn1;
        elf(6) = f6;
    end
end
end



