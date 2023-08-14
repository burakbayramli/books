function coeffcnt(ielem,item,model,ndf,npe,time,ntype,ne,f3,mxelm,elx)
% c     __________________________________________________________________
% c
% c     the subroutine is called in main to compute coefficient matrices
% c      and source vector for the model problem in eq. (1) (see main)
% c
% c        x.........  global (i.e., problem) coordinate
% c        xi .......  local (i.e., element) coordinate
% c        h.........  element length
% c        {sf}......  element interpolation (or shape) functions
% c        {gdsf}....  first derivative of sf w.r.t. x
% c        {gddsf}...  second derivative of sf w.r.t. x
% c        gj........  determinant of the jacobian matrix [j]
% c        [gauspt]..  4x4 matrix of gauss points: n-th column corresponds
% c                    to the n-point gauss rule
% c        [gauswt]..  4x4 matrix of gauss weights (see the comment above)
% c        [a],[b],..  element matrices needed to compute elk
% c        [elk].....  element coefficient matrix [k]
% c        [elm].....  element 'mass' matrix [m]
% c     __________________________________________________________________
% c
% implicit real*8(a-h,o-z)
% common/stf1/elk(9,9),elm(9,9),elf(9),elx(4),elu(9),elv(9),ela(9)
% common/stf2/a1:a2,a3,a4,a5,ax0,ax1:bx0,bx1:cx0,cx1:ct0,ct1:fx0,fx1:fx2
% common/shp/sf(4),gdsf(4),gddsf(4),gj
% dimension gauspt(5,5),gauswt(5,5),f3(mxelm)

% data gauspt/5*0.0d0,-.57735027d0,.57735027d0,3*0.0d0,-.77459667d0,
% * 0.0d0,.77459667d0,2*0.0d0,-.86113631d0,-.33998104d0,.33998104d0,
% *.86113631d0,0.0d0,-.906180d0,-.538469d0,0.0d0,.538469d0,.906180d0/
% c
% data gauswt/2.0d0,4*0.0d0,2*1.0d0,3*0.0d0,.55555555d0,.88888888d0,
% * 0.55555555d0,2*0.0d0,.34785485d0,2*.65214515d0,.34785485d0,0.0d0,
% * 0.236927d0,.478629d0,.568889d0,.478629d0,.236927d0/

gauspt=[0.0, 0.0, 0.0, 0.0, 0.0;
    -0.57735027, 0.57735027, 0.0, 0.0, 0.0;
    -0.77459667, 0.0, 0.77459667, 0.0, 0.0;
    -0.86113631,-0.33998104, 0.33998104, 0.86113631, 0.0;
    -0.90617984,-0.53846931,0.0,0.53846931,0.90617984]';
gauswt=[2.0 0.0 0.0 0.0 0.0 ;
    1.0, 1.0, 0.0, 0.0, 0.0;
    0.55555555, 0.88888888, 0.55555555, 0.0, 0.0;
    0.34785485, 0.65214515, 0.65214515, 0.34785485, 0.0d0;
    0.23692688, 0.47862867, 0.56888888, 0.47862867, 0.23692688]';

global elk elf elm elu ela elv
global a1 a2 a3 a4 a5 ax0 ax1 bx0 bx1 cx0 cx1 ct0 ct1 fx0 fx1 fx2
nn=ndf*npe;
h = elx(npe)-elx(1);
if(ielem==0)
    ngp=4;
else
    ngp=ielem+1;
end

for j=1:nn
    if(item<=2)
        elf(j)=0.0;
    end
    for i=1:nn
        if(item>=0)
            elm(i,j)=0.0;
        end
        elk(i,j)=0.0;
    end
end

if(model~=2) %c     for-loop on number of gauss points begins here
    for ni=1:ngp
        xi = gauspt(ni,ngp);
        %c  call subroutine shape1d to evaluate the interpolation functions
        %c  and their global derivatives at the gauss point xi
        [gj,sf,dsf,gdsf,gddsf]=shape1d(h,ielem,npe,xi);
        const=gj*gauswt(ni,ngp);
        if(ielem==0)
            x=elx(1)+0.5*h*(1.0+xi);
        else
            x=0.0;
            for j=1:npe
                x=x+sf(j)*elx(j);
            end
        end
        %c  compute coefficient matrices and vectors for vaious model problems
        %c  governed by single second-order and fourth-order equations
        %c  (model = 1 or 3; ntype = 0 or 1)
        cx=cx0+cx1*x;
        if(item~=3)
            fx=fx0+fx1*x+fx2*x*x;
        end
        if(item>0)
            ct=ct0+ct1*x;
        end
        if(model==1)     %c     coefficients for all single-variable problems (model=1)
            if(ntype==0) %c     all problems governed by model equation (3.1) (ntype=0)
                ax=ax0+ax1*x;
                for j = 1:nn
                    if(item<=2)
                        elf(j)=elf(j)+const*sf(j)*fx;
                    end
                    for i=1:nn
                        if(item~=0)
                            elm(i,j)=elm(i,j)+const*sf(i)*sf(j)*ct;
                        end
                        aij=const*gdsf(i)*gdsf(j);
                        cij=const*sf(i)*sf(j);
                        elk(i,j)=elk(i,j)+ax*aij+cx*cij;
                    end
                end
            else  %c radially symmetric elasticity problems (model=1: ntype>0)
                %c ax0=e1: ax1=e2, bx0=nu12, bx1=h, thickness
                anu21=bx0*ax0/ax1;
                if(ntype==1)
                    c11=bx1*ax0/(1.0-bx0*anu21);
                    c22=c11*(ax1/ax0);
                    c12=bx0*c22;
                else
                    denom=1.0-bx0-anu21;
                    c11=bx1*ax0*(1.0-bx0)/(1.0+bx0)/denom;
                    c22=bx1*ax1*(1.0-anu21)/(1.0+anu21)/denom;
                    c12=bx0*c22;
                end
                for j=1:nn
                    if(item<=2)
                        elf(j)=elf(j)+const*sf(j)*fx*x;
                    end
                    for i=1:nn
                        if(item~=0)
                            elm(i,j)=elm(i,j)+const*sf(i)*sf(j)*ct*x;
                        end
                        aij=const*gdsf(i)*gdsf(j)*c11*x;
                        cij=const*sf(i)*sf(j)*cx*x;
                        dij=const*(gdsf(i)*sf(j)+sf(i)*gdsf(j))*c12;
                        eij=const*sf(i)*sf(j)*c22/x;
                        elk(i,j)=elk(i,j)+aij+cij+dij+eij;
                    end
                end
            end
        else
            %c     coefficients for the euler-bernoulli theory (model=2)
            if(ntype==0) 
                %c     the euler-bernoulli beam element (model=1 and ntype=0)
                bx=bx0+bx1*x;
                cx=cx0+cx1*x;
                for j = 1:nn
                    if(item<=2)
                        elf(j)=elf(j)+const*sf(j)*fx;
                    end
                    for i = 1:nn
                        if(item>0)
                            if(item<=3)
                                elm(i,j)=elm(i,j)+const*sf(i)*sf(j)*ct;
                            else
                                elm(i,j)=elm(i,j)+const*gdsf(i)*gdsf(j);
                            end
                        end
                        bij=const*gddsf(i)*gddsf(j);
                        cij=const*sf(i)*sf(j);
                        elk(i,j)=elk(i,j)+bx*bij+cx*cij;
                    end
                end
            else
                %c    the e-b circular plate element (model=1 and ntype>0)
                anu21=bx0*ax0/ax1;
                di=(bx1^3)/12.0;
                d11=di*ax0/(1.0-bx0*anu21);
                d22=d11*(ax1/ax0);
                d12=bx0*d22;
                for j=1:nn
                    if(item<=2)
                        elf(j) = elf(j) + const*sf(j)*fx*x;
                    end
                    for i=1:nn
                        bij=const*gddsf(i)*gddsf(j)*d11*x;
                        cij=const*sf(i)*sf(j)*cx*x;
                        dij=const*(gddsf(i)*gdsf(j)+gdsf(i)*gddsf(j))*d12;
                        eij=const*gdsf(i)*gdsf(j)*d22/x;
                        elk(i,j)=elk(i,j)+bij+cij+dij+eij;
                    end
                end
            end
        end
    end
else
    %c     coefficients for the timoshenko beam and circular plate (model=2)
    %c     full integration for bending coefficients
    for ni=1:ngp
        xi=gauspt(ni,ngp);
        [gj,sf,dsf,gdsf,gddsf]=shape1d(h,ielem,npe,xi);
        const=gj*gauswt(ni,ngp);
        x = 0.0;
        for j=1:npe
            x=x+sf(j)*elx(j);
        end
        if((ntype==0)||(ntype==2))    
            %c         the timoshenko beam element (model=2 and ntype=0 or 2)
            bx=bx0+bx1*x;
            cx=cx0+cx1*x;
            fx=fx0+fx1*x+fx2*x*x;
            jj=1;
            for j=1:npe
                if(item<=2)
                    elf(jj)=elf(jj)+fx*sf(j)*const;
                end
                ii=1;
                for i=1:npe
                    cij=sf(i)*sf(j)*const;
                    bij=gdsf(i)*gdsf(j)*const;
                    elk(ii,jj)=elk(ii,jj)+cx*cij;
                    elk(ii+1,jj+1)=elk(ii+1,jj+1)+bx*bij;
                    if(item~=0)
                        elm(ii,jj)=elm(ii,jj)+ct0*cij;
                        elm(ii+1,jj+1)=elm(ii+1,jj+1)+ct1*cij;
                    end
                    ii=ndf*i+1;
                end
                jj=ndf*j+1;
            end
        else
            %c     timoshenko circular plate element (model=2 and ntype=1 or 3)
            %c     ax0=e1: ax1=e2, bx0=anu12, bx1=h
            anu21=bx0*ax0/ax1;
            cx=cx0+cx1*x;
            fx=fx0+fx1*x;
            di=(bx1^3)/12.0;
            d11=di*ax0/(1.0-bx0*anu21);
            d22=d11*(ax1/ax0);
            d12=bx0*d22;
            jj=1;
            for j=1:npe
                if(item<=2)
                    elf(jj)=elf(jj)+fx*sf(j)*const*x;
                end
                ii=1;
                for i=1:npe
                    bij=const*gdsf(i)*gdsf(j)*d11*x;
                    cij=const*sf(i)*sf(j)*x;
                    dij=const*(gdsf(i)*sf(j)+sf(i)*gdsf(j))*d12;
                    eij=const*sf(i)*sf(j)*d22/x;
                    elk(ii,jj)=elk(ii,jj)+ cx*cij;
                    elk(ii+1,jj+1)=elk(ii+1,jj+1)+ bij + dij + eij;
                    if(item~=0)
                        elm(ii,jj)=elm(ii,jj)+ct0*cij;
                        elm(ii+1,jj+1)=elm(ii+1,jj+1)+ct1*cij;
                    end
                    ii=ndf*i+1;
                end
                jj=ndf*j+1;
            end
        end
    end
    lgp=ngp-1;
    for ni=1:lgp  %     c     reduced integration is used to evaluate the transverse shear terms
        xi=gauspt(ni,lgp);
        [gj,sf,dsf,gdsf,gddsf]=shape1d(h,ielem,npe,xi);
        const=gj*gauswt(ni,lgp);
        x = 0.0;
        for j=1:npe
            x = x + sf(j)*elx(j);
        end
        if(ntype==0)||(ntype==2)
            %c  the timoshenko beam element (model=2 and ntype=0 or 2)
            %c  ax = gak = ax0 + ax1*x  (reduced integration)
            ax=ax0+ax1*x;
            jj=1;
            for j=1:npe
                ii=1;
                for i=1:npe
                    b11=gdsf(i)*gdsf(j)*const;
                    b01=sf(i)*gdsf(j)*const;
                    b10=gdsf(i)*sf(j)*const;
                    b00=sf(i)*sf(j)*const;
                    elk(ii,jj)    =elk(ii,jj)    +ax*b11;
                    elk(ii,jj+1)  =elk(ii,jj+1)  +ax*b10;
                    elk(ii+1,jj)  =elk(ii+1,jj)  +ax*b01;
                    elk(ii+1,jj+1)=elk(ii+1,jj+1)+ax*b00;
                    ii=i*ndf+1;
                end
                jj=j*ndf+1;
            end
        else
            %c     timoshenko circular plate element (model=2 and ntype=1 or 3)
            %c     bx1=h, fx2=g13*k (reduced integration)
            a33=bx1*fx2;
            jj=1;
            for j=1:npe
                ii=1;
                for i=1:npe
                    bij = const*gdsf(i)*gdsf(j)*x;
                    cij = const*sf(i)*sf(j)*x;
                    dij = const*gdsf(i)*sf(j)*x;
                    dji = const*sf(i)*gdsf(j)*x;
                    elk(ii,jj)    =elk(ii,jj)     + a33*bij;
                    elk(ii,jj+1)  =elk(ii,jj+1)   + a33*dij;
                    elk(ii+1,jj)  =elk(ii+1,jj)   + a33*dji;
                    elk(ii+1,jj+1)=elk(ii+1,jj+1) + a33*cij;
                    ii=ndf*i+1;
                end
                jj=ndf*j+1;
            end
        end
    end
    if(item==0)&&(ntype>1)
        call timforce(elf,elx,fx0,fx1,fx2,h,ntype,ne,f3,mxelm);
    end
end

if (item>2)
    return
end
if(item==1)||(item==2) %c     equivalent coefficient matrices for time-dependent problems
    if(item == 1)      %c     alfa-family of time approximation for parabolic equations
        for j=1:nn
            sum=0.0;
            for i=1:nn
                sum=sum+(elm(i,j)-a2*elk(i,j))*elu(i);
                elk(i,j)=elm(i,j)+a1*elk(i,j);
            end
            elf(j)=(a1+a2)*elf(j)+sum;
        end
    else              %c     newmark-family of approximation for hyperbolic equations
        if(time==0.0)
            for j=1:nn
                for i=1:nn
                    elf(j)=elf(j)-elk(i,j)*elu(i);
                    elk(i,j)=elm(i,j);
                end
            end
        else
            for j=1:nn
                for i=1:nn
                    elf(j)=elf(j)+elm(i,j)*(a3*elu(i)+a4*elv(i)+a5*ela(i));
                    elk(i,j)=elk(i,j)+a3*elm(i,j);
                end
            end
        end
    end
end
return
end

