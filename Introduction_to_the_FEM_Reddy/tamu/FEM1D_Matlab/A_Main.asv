clear all
clc
% c      program name: fem1d          length(including blanks):----- lines
% c
% c      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
% c      *                       program  fem1d                          *
% c      *         (a finite element analysis computer program)          *
% c      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
% c       _______________________________________________________________
% c      |                                                               |
% c      |  this is a finite element computer program for the analysis   |
% c      |  of following three model equations and others:               |
% c      |                                                               |
% c      |  1. heat transfer, fluid mechanics, bars, and cables:         |
% c      |                                                               |
% c      |            ct.u* + ct.u^ - (ax.u')' + cx.u = fx              |
% c      |                                                               |
% c      |  2. the timoshenko beam and circular plate theory:            |
% c      |                                                               |
% c      |             ct0.w^ - [ax.(w' + s)]' + cx.w = fx              |
% c      |             ct1.s^ - (bx.s')' + ax.(w' + s) = 0              |
% c      |                                                               |
% c      |  3. the euler-bernoulli beam and circular plate theory:       |
% c      |                                                               |
% c      |               ct.w^ + (bx.w'')'' + cx.w = fx                 |
% c      |                                                               |
% c      | in the above equations  (')  and (*) denote  differentiations |
% c      | with respect to space x and time t,  and  ax, bx, cx, ct, and |
% c      | fx are functions of x only:                                   |
% c      |                                                               |
% c      |    ax = ax0 + ax1.x,  bx = bx0 + bx1.x,  cx = cx0 + cx1.x     |
% c      |         ct = ct0 + ct1.x,  fx = fx0 + fx1.x + fx2.x.x         |
% c      |                                                               |
% c      |    in addition to the three model equations, other equations  |
% c      | (for example, disks, trusses, and frames) can be analyzed by  |
% c      | the program.                                                  |
% c      |_______________________________________________________________|
% c
% c      _________________________________________________________________
% c      .                                                               .
% c      .               key variables used in the program               .
% c      . see table 7.3.2 of the book for a description of the variables.
% c      .                                                               .
% c      . ndf....... number of degrees of freeform per node              .
% c      . neq....... number of equations in the model (before b. c.)    .
% c      . ngp....... number of gauss points used in the evaluation of   .
% c      .            the element coeffcients,  elk ,  elf ,  elm        .
% c      . nhbw...... half bandwidth of global coefficient matrix  glk   .
% c      . nn ....... number of total degrees of freeform in the element  .
% c      . npe....... number of nodes per element                        .
% c      _________________________________________________________________
% c      .                                                               .
% c      .          dimensions of various arrays in the program          .
% c      .                                                               .
% c      . values of mxelm,mxnod, etc. in the parameter statement should .
% c      .         be changed to meet the requirements of problem:       .
% c      .                                                               .
% c      . mxelm..... maximum number of elements in the mesh:            .
% c      . mxebc..... maximum number of speci. primary deg. of freeform   .
% c      . mxmbc..... maximum number of speci. mixed boundary conditions .
% c      . mxnbc..... maximum number of speci. secondary deg. of freeform .
% c      . mxneq..... maximum number of equations in the fe model        .
% c      . mxnod..... maximum number of nodes in the mesh                .
% c      .                                                               .
% c      . note:  the following dimension statement in subroutine jacobi .
% c      .        should be modified when mxneq is greater than 500:     .
% c      .         dimension  v(500,500),vt(500,500),w(500,500),ih(500)  .
% c      .        the value of mxneq should be used in place of '500'    .
% c      _________________________________________________________________
% c      .                                                               .
% c      .                subroutines used in the program                .
% c      .                                                               .
% c      .  assemble, boundary, coeffcnt, constrnt, echodata, eqnsolvr,  .
% c      .    eignslvr, jacobi, matrxmlt, mesh1d, postproc, reaction,    .
% c      .             shape1d, timforce, timstres, transfrm             .
% c      _________________________________________________________________
% c
% implicit real*8(a-h,o-z)
% parameter  (mxelm=250,mxneq=500,mxebc=20,mxnbc=20,mxmbc=20,mxnod=250,mxmpc=5)
% dimension  dcax(mxelm,2),dcbx(mxelm,2),dccx(mxelm,2),dcfx(mxelm,3)
% dimension  gu0(mxneq),gu1(mxneq),gu2(mxneq),gpu(mxneq),dx(mxnod)
% dimension  ibdy(mxebc),ispv(mxebc,2),issv(mxnbc,2),inbc(mxmbc,2)
% dimension  imc1(mxmpc,2),imc2(mxmpc,2),vmpc(mxmpc,4)
% dimension  icon(9),vcon(9),trm(mxneq,mxneq)
% dimension  glm(mxneq,mxneq),glf(mxneq),glx(mxnod),nod(mxelm,4)
% dimension  cs(mxelm),sn(mxelm),cnt(mxelm),snt(mxelm),xb(mxelm)
% dimension  egnval(mxneq),egnvec(mxneq,mxneq),glk(mxneq,mxneq)
% dimension  pr(mxelm),se(mxelm),sl(mxelm),sa(mxelm),si(mxelm)
% dimension  hf(mxelm),vf(mxelm),pf(mxelm),f3(mxelm),title(20)
% dimension  uref(mxmbc),vspv(mxebc),vssv(mxnbc),vnbc(mxmbc)
% common/stf1/elk(9,9),elm(9,9),elf(9),elx(4),elu(9),elv(9),ela(9)
% common/stf2/a1,a2,a3,a4,a5,ax0,ax1,bx0,bx1,cx0,cx1,ct0,ct1,fx0,fx1,fx2
% common/io/in,it
%--------------------------------------------------------------
% c             _______________________________________________
% c            |                                               |
% c            |       p r e p r o c e s s o r   u n i t       |
% c            |_______________________________________________|
% c
%--------------------------------------------------------------
global glk glf glm elk elm elf pv_solution elu ela elv
global a1 a2 a3 a4 a5 ax0 ax1 bx0 bx1 cx0 cx1 ct0 ct1 fx0 fx1 fx2
mxelm=250;mxneq=500;mxebc=20;mxnbc=20;mxmbc=20;mxnod=250;mxmpc=5;
title=zeros(20,1); icon=zeros(9,1); vcon=zeros(9,1);
ibdy=zeros(mxebc,1); ispv=zeros(mxebc,2); issv=zeros(mxnbc,2); 
inbc=zeros(mxmbc,2); imc1=zeros(mxmpc,2); imc2=zeros(mxmpc,2); 
vmpc=zeros(mxmpc,4); uref=zeros(mxmbc,1); vspv=zeros(mxebc,1); 
vssv=zeros(mxnbc,1); vnbc=zeros(mxmbc,1);
%--------------------------------------------------------------
neqr=0;
alfa=0;
dt=0;
nt=0;
nssv=0;
jvec=1;
time=0.0d0;
tolrns=1.0d-06;
%--------------------------------------------------------------
% open (in,file=' ')
% open (it,file=' ')
addpath('Subroutines\'); addpath('InputFiles\')
prompt = {'Enter Input File Dir./Name :','Enter Output File Dir./Name:'};
dlg_title = 'Input';
num_lines = 1;
def = {'*.inp','*.out'};
answer = inputdlg(prompt,dlg_title,num_lines,def);
answer1_str=answer{1};   %convert cell to string
answer2_str=answer{2};
inputfile_name=sprintf('%s',answer1_str);
outputfile_name=sprintf('%s',answer2_str);
fid_in =fopen(inputfile_name,'r');
fid_out=fopen(outputfile_name,'w');
if(fid_in<=0)
    fprintf('\n**** Error: \n\t\tCannot Read Input File:  %s - Exiting   ****\n',inputfile_name);
end
if(fid_out<=0)
    fprintf('\n**** Error: \n\t\tCannot Write Output File:%s - Exiting ****\n',outputfile_name);
end
if (fid_in>0)&&(fid_out>0)
    % fid_in =fopen('Input_Files/Ex6-2-2.inp','r');
    % fid_out = fopen('Output_Files/Ex6-2-2.out','w');
    fprintf(fid_out,'\t\t*** ECHO OF THE INPUT DATA STARTS ***\n\n');
    fprintf('\t*** ECHO OF THE INPUT DATA STARTS ***\n');
    data_input=fread(fid_in);
    fwrite(fid_out,data_input);
    fprintf(fid_out,'\n\n\t\t*** ECHO OF THE INPUT DATA ENDS ***\n\n');
    fprintf('\t*** ECHO OF THE INPUT DATA ENDS ***\n\n');
    fclose(fid_out);
    fid_out=fopen(outputfile_name,'a');
    [data,results] = readtext(inputfile_name, '[,\s\t]', '', '','numeric');
    isf=isfinite(data);  % This means not Inf, -Inf or NaN
    data=data.';
    isf=isf.';
    datacol=data(:);
    isfcol=isf(:);
    colleng=length(isfcol);
    L=0;
    for k=1:colleng,
        if isfcol(k)==1,
            L=L+1;
            datafinal(L)=datacol(k);
        end
    end
    datafinal.';
    %--------------------------------------------------------------
    % echodata(fid_inn,it)
    %c
    model=datafinal(1);
    ntype=datafinal(2);
    item=datafinal(3);
    ielem=datafinal(4);
    nem=datafinal(5);
    icont=datafinal(6);
    nprnt=datafinal(7);
    valread=7;
    %--------------------------------------------------------------
    % Memory Allocations
    dcax=zeros(nem,2); dcbx=zeros(nem,2); dccx=zeros(nem,2); dcfx=zeros(nem,3);
    pr=zeros(nem,1); se=zeros(nem,1); sl=zeros(nem,1); sa=zeros(nem,1); 
    si=zeros(nem,1); hf=zeros(nem,1); vf=zeros(nem,1); pf=zeros(nem,1); 
    f3=zeros(nem,1); nod=zeros(nem,4); cs=zeros(nem,1); sn=zeros(nem,1); 
    cnt=zeros(nem,1); snt=zeros(nem,1); xb=zeros(nem,1);
    %--------------------------------------------------------------
    if(model>=3)
        npe=2;
        if(model==4)&&(ntype>=1)
            ndf=3;
        else
            ndf=2;
        end
        if(model==4)&&(ntype==2)
            ielem=1;
        else
            ielem=0;
        end
    else
        if(model==2)
            ndf=2;
            if(ntype>1)
                ielem=1;
            end
        else
            ndf=1;
        end
        npe=ielem+1;
    end
    % ------------------------------------------------------------------
    %c     data input for bar-like and beam problems (model=1:2, and 3)
    if(model~=4)
        if(icont~=0)
            nnm = nem*(npe-1)+1;
            nem1= nem + 1;
            for i=1:nem1
                valread=valread+1;
                dx(i)=datafinal(valread); % read(in,*) (dx(i), i=1:nem1)
            end
            [glx,nod]=mesh1d(nem,npe,mxelm,mxnod,dx);
            ax0=datafinal(valread+1);       % read(in,*) ax0,ax1
            ax1=datafinal(valread+2);
            bx0=datafinal(valread+3);       % read(in,*) bx0,bx1
            bx1=datafinal(valread+4);
            cx0=datafinal(valread+5);       % read(in,*) cx0,cx1
            cx1=datafinal(valread+6);
            valread=valread+6;
            if(item<3)
                fx0=datafinal(valread+1);     % read(in,*) fx0,fx1,fx2
                fx1=datafinal(valread+2);
                fx2=datafinal(valread+3);
                valread=valread+3;
            end
        else
            % read glx, nod, and element-wise continuous coefficients [dc.x]
            % read(in,*)nnm
            valread=valread+1;
            nnm=datafinal(valread);
            for n=1:nem
                for i=1:npe
                    valread=valread+1;
                    nod(n,i)=datafinal(valread);
                end
                valread=valread+1;
                glx(n)=datafinal(valread);
                for i=1:2
                    valread=valread+1;
                    dcax(n,i)=datafinal(valread);
                end
                for i=1:2
                    valread=valread+1;
                    dcbx(n,i)=datafinal(valread);
                end
                for i=1:2
                    valread=valread+1;
                    dccx(n,i)=datafinal(valread);
                end
                for i=1:3
                    valread=valread+1;
                    dcfx(n,i)=datafinal(valread);
                end
                % read(in,*) (nod(n,i),i=1:npe), glx(n)
                % read(in,*) (dcax(n,i),i=1:2)
                % read(in,*) (dcbx(n,i),i=1:2)
                % read(in,*) (dccx(n,i),i=1:2)
                % read(in,*) (dcfx(n,i),i=1:3)
            end
        end
    else
        %c     input data for plane truss or frame structures (model=4)
        valread=valread+1;
        nnm=datafinal(valread);     % read(in,*)nnm
        if(ntype~=0)
            for n=1:nem
                pr(n)=datafinal(valread+1);
                se(n)=datafinal(valread+2);
                sl(n)=datafinal(valread+3);
                sa(n)=datafinal(valread+4);
                si(n)=datafinal(valread+5);
                cs(n)=datafinal(valread+6);
                sn(n)=datafinal(valread+7);
                hf(n)=datafinal(valread+8);
                vf(n)=datafinal(valread+9);
                pf(n)=datafinal(valread+10);
                xb(n)=datafinal(valread+11);
                cnt(n)=datafinal(valread+12);
                snt(n)=datafinal(valread+13);
                valread=valread+13;
                for i=1:2
                    valread=valread+1;
                    nod(n,i)=datafinal(valread);
                end
                %  read(in,*) pr(n),se(n),sl(n),sa(n),si(n),cs(n),sn(n)
                %  read(in,*) hf(n),vf(n),pf(n),xb(n),cnt(n),snt(n)
                %  read(in,*) (nod(n,i),i=1:2)
            end
        else
            for n=1:nem
                se(n)=datafinal(valread+1);
                sl(n)=datafinal(valread+2);
                sa(n)=datafinal(valread+3);
                cs(n)=datafinal(valread+4);
                sn(n)=datafinal(valread+5);
                hf(n)=datafinal(valread+6);
                valread=valread+6;
                for i=1:2
                    valread=valread+1;
                    nod(n,i)=datafinal(valread);
                end
                %  read(in,*) se(n),sl(n),sa(n),cs(n),sn(n),hf(n)
                %  read(in,*) (nod(n,i),i=1:2)
            end
        end
        ncon=datafinal(valread+1);     %  read(in,*) ncon
        valread=valread+1;
        if(ncon~=0)
            for i=1: ncon
                icon(i)=datafinal(valread+1);
                vcon(i)=datafinal(valread+2);
                valread=valread+2;  %  read(in,*) icon(i),vcon(i)
            end
        end
        
    end
    neq=nnm*ndf;
    dx=zeros(nnm,1);gu0=zeros(neq,1); gu1=zeros(neq,1); gu2=zeros(neq,1);
    gpu=zeros(neq,1); egnval=zeros(neq,1); egnvec=zeros(neq,neq);
    glk=zeros(neq,neq);glm=zeros(neq,neq); glf=zeros(neq,1);trm=zeros(neq+10,neq+10);
    pv_solution=zeros(neq,1);
    %c     read data on boundary conditions of three kinds: dirichlet (pv)
    %c     neumann (sv), and newton's (mixed) types
    valread=valread+1;
    nspv=datafinal(valread);  %read(in,*) nspv
    if(nspv~=0)
        for nb=1:nspv
            if(item>2)
                for j=1:2
                    valread=valread+1;
                    ispv(nb,j)=datafinal(valread);
                end
            else
                for j=1:2
                    valread=valread+1;
                    ispv(nb,j)=datafinal(valread);
                end
                valread=valread+1;
                vspv(nb)=datafinal(valread);    %read(in,*) (ispv(nb,j),j=1:2),vspv(nb)
            end
        end
    end
    %c
    if(item<=2)
        valread=valread+1;
        nssv=datafinal(valread);  %read(in,*) nssv
        if(nssv~=0)
            for ib=1:nssv
                for j=1:2
                    valread=valread+1;
                    issv(ib,j)=datafinal(valread);
                end
                valread=valread+1;
                vssv(ib)=datafinal(valread);%read(in,*) (issv(ib,j),j=1:2),vssv(ib)
            end
        end
    end
    %c
    valread=valread+1;
    nnbc=datafinal(valread);  %     read(in,*) nnbc
    if(nnbc~=0)
        for i=1: nnbc
            for j=1:2
                valread=valread+1;
                inbc(i,j)=datafinal(valread);
            end
            vnbc(i)=datafinal(valread+1);
            uref(i)=datafinal(valread+2);
            valread=valread+2;
            %read(in,*) (inbc(i,j),j=1:2),vnbc(i),uref(i)
        end
    end
    %c
    %c  read data on multi-point constraints
    valread=valread+1;
    nmpc=datafinal(valread);  %read(in,*) nmpc
    if(nmpc~=0)
        for i=1:nmpc
            for j=1:2
                valread=valread+1;
                imc1(i,j)=datafinal(valread);
            end
            for j=1:2
                valread=valread+1;
                imc2(i,j)=datafinal(valread);
            end
            for j=1:4
                valread=valread+1;
                vmpc(i,j)=datafinal(valread);
            end
            %             read(in,*)(imc1(i,j),j=1:2),(imc2(i,j),j=1:2),(vmpc(i,j),j=1:4)
        end
    end
    %c
    if(item ~= 0)
        %c     input data here for time-dependent problems
        if(item<=3)
            valread=valread+1;
            ct0=datafinal(valread);
            ct1=datafinal(valread+1);
            valread=valread+1;
            % read(in,*) ct0,ct1
        end
        if(item<=2)
            dt=datafinal(valread+1);
            alfa=datafinal(valread+2);
            gama=datafinal(valread+3);
            incond=datafinal(valread+4);
            ntime=datafinal(valread+5);
            intvl=datafinal(valread+6);
            a1=alfa*dt;
            a2=(1.0-alfa)*dt;
            valread=valread+6;
            if(incond~=0)
                for i=1:neq
                    valread=valread+1;
                    gu0(i)=datafinal(valread); % read(in,*) (gu0(i),i=1:neq);
                end
            else
                for i=1:neq
                    gu0(i)=0.0;
                end
            end
            if(item==2)
                a3=2.0/gama/(dt*dt);
                a4=a3*dt;
                a5=1.0/gama-1.0;
                if(incond~=0)
                    for i=1:neq
                        valread=valread+1;
                        gu1(i)=datafinal(valread); %read(in,*) (gu1(i),i=1:neq);
                    end
                else
                    for i=1:neq
                        gu1(i)=0.0;
                        gu2(i)=0.0;
                    end
                end
            end
        end
    end
    fprintf('\t*** COMPLETED READING OF INPUT DATA ***\n\n');
    %c
    %c    ----------------------------------------------------------------
    %c      e  n  d     o  f     t  h  e     i  n  p  u  t      d  a  t  a
    %c     ----------------------------------------------------------------
    %c    compute the half bandwidth of the coefficient matrix glk
    %c
    nhbw=0.0;
    for n=1:nem
        for i=1:npe
            for j=1:npe
                nw=(abs(nod(n,i)-nod(n,j))+1)*ndf;
                if(nhbw<nw)
                    nhbw=nw;
                end
            end
        end
    end
    %c
    fprintf(fid_out,'______________________________________________________________________________\n\n');
    fprintf(fid_out,'               OUTPUT from program   FEM1D   by J N REDDY \n');
    fprintf(fid_out,'______________________________________________________________________________\n');
    fprintf(fid_out,'\n\t ANALYSIS OF MODEL %d, AND TYPE %d PROBLEM \n',model,ntype);
    fprintf(fid_out,'\t  (**********see the code below**********)\n\n');
    fprintf(fid_out,'\tmodel=1:ntype=0: A problem described by model EQ. 1\n');
    fprintf(fid_out,'\tmodel=1:ntype=1: A circular disk (PLANE STRESS) \n');
    fprintf(fid_out,'\tmodel=1:ntype>1: A circular disk (PLANE STRAIN) \n');
    fprintf(fid_out,'\tmodel=2,ntype=0: A Timoshenko BEAM (RIE) problem\n');
    fprintf(fid_out,'\tmodel=2,ntype=1: A Timoshenko PLATE (RIE) problem\n');
    fprintf(fid_out,'\tmodel=2,ntype=2: A Timoshenko BEAM (CIE) problem\n');
    fprintf(fid_out,'\tmodel=2,ntype>2: A Timoshenko PLATE (CIE) problem\n');
    fprintf(fid_out,'\tmodel=3,ntype=0: A Euler-Bernoulli BEAM problem\n');
    fprintf(fid_out,'\tmodel=3,ntype>0: A Euler-Bernoulli circular PLATE \n');
    fprintf(fid_out,'\tmodel=4,ntype=0: A plane TRUSS problem\n');
    fprintf(fid_out,'\tmodel=4,ntype=1: A Euler-bernoulli FRAME problem\n');
    fprintf(fid_out,'\tmodel=4,ntype=2: A Timoshenko (CIE) FRAME problem\n');
    %c             _______________________________________________
    %c            |                                               |
    %c            |          p r o c e s s o r   u n i t          |
    %c            |_______________________________________________|
    %c
    %c     time marching scheme begins here. for  item=2, initial conditions
    %c     on second derivatives of the solution are computed in the program
    %c
    fprintf('\t*** IN PROCESSOR UNIT ***\n');
    if(item~=0)
        if(item==1)
            nt=nt+1;
            time=time+dt;
        end
    end
    %c
    if(item>=3)
        nhbw=neq;
    end
    %c
    %c  initialize global matrices and vectors
    ianalysis=0;
    while (ianalysis==0) %line 150
        for i=1:neq
            glf(i)=0.0;
            for j=1:nhbw
                if(item>=3)
                    glm(i,j)=0.0;
                end
                glk(i,j)=0.0;
            end
        end
        %c for-loop for element calculations and assembly
        for ne = 1: nem
            if(model~=4)
                if(icont~=1)
                    ax0=dcax(ne,1); ax1=dcax(ne,2);
                    bx0=dcbx(ne,1); bx1=dcbx(ne,2);
                    cx0=dccx(ne,1); cx1=dccx(ne,2);
                    fx0=dcfx(ne,1); fx1=dcfx(ne,2); fx2=dcfx(ne,3);
                end
                %c
                l=0;
                for i=1:npe
                    ni=nod(ne,i);
                    if(icont==1)
                        elx(i)=glx(ni);
                    else
                        elx(1)=0.0;
                        elx(2)=0.5*glx(ne);
                        elx(npe)=glx(ne);
                    end
                    if(item==1||item==2)
                        li=(ni-1)*ndf;
                        for j=1:ndf
                            li=li+1;
                            l=l+1;
                            elu(l)=gu0(li);
                            if(item==2)&&(nt>0)
                                elv(l)=gu1(li);
                                ela(l)=gu2(li);
                            end
                        end
                    end
                end
                coeffcnt(ielem,item,model,ndf,npe,time,ntype,ne,f3,mxelm,elx);
            else
                transfrm(mxelm,ne,ntype,pr,se,sl,sa,si,cs,sn,cnt,snt,hf,vf,pf,xb);
            end
            if(nprnt~=0)
                nn = npe*ndf;
                if(nprnt <=2)
                    if(ne<=5)&&(nt<=1) % write(it,550)
                        fprintf(fid_out,'\nElement coefficient matrix, [ELK]:\n');
                        for i=1:nn
                            for j=1:nn
                                fprintf(fid_out,'\t% 3.5e',elk(i,j));
                            end
                            fprintf(fid_out,'\n'); % write(it,540) (elk(i,j),j=1:nn)
                        end
                        if(item>=3) % write(it,360)
                            fprintf(fid_out,'\nElement coefficient matrix, [ELM]:\n');
                            for i=1:nn
                                for j=1:nn
                                    fprintf(fid_out,'\t% 3.5e',elm(i,j));
                                end
                                fprintf(fid_out,'\n');
                            end
                        else
                            fprintf(fid_out,'\nElement source vector, {ELF}:\n');
                            for i=1:nn
                                fprintf(fid_out,'\t% 3.5e',elf(i));
                            end
                            fprintf(fid_out,'\n');
                        end
                    end
                end
            end
            %c  assemble element matrices
            assemblesub(nod,mxelm,mxneq,ndf,npe,ne,item);
            %c
        end
        %c  call subroutine constrnt to impose constraint boundary conditions,
        %c  for example, inclined support conditions
        if(model==4)
            if(ncon~=0)
                constrnt(neq,nhbw,ndf,ncon,icon,vcon,trm,mxneq)
            end
        end
        %c  impose multi-point constraints using the penalty method
        if(nmpc~=0)
            if(nprnt==2)
                fprintf(fid_out,'\nGlobal coefficient matrix, [GLK]:'); % write(it,570)
                for i=1:neq
                    for j=1:nhbw
                        fprintf(fid_out,'\t% 3.5e',glk(i,j));
                    end
                    fprintf(fid_out,'\n');
                end
            end
            vmax=0.0;
            for i=1:neq
                for j=i:nhbw
                    value=abs(glk(i,j));
                    if(value>vmax)
                        vmax=value;
                    end
                end
            end
            pnlty=vmax*1.0e4;
            for nc=1:nmpc
                nforf1=(imc1(nc,1)-1)*ndf+imc1(nc,2);
                nforf2=(imc2(nc,1)-1)*ndf+imc2(nc,2);
                glk(nforf1,1)=glk(nforf1,1)+pnlty*vmpc(nc,1)*vmpc(nc,1);
                glk(nforf2,1)=glk(nforf2,1)+pnlty*vmpc(nc,2)*vmpc(nc,2);
                glf(nforf1)=glf(nforf1)+pnlty*vmpc(nc,1)*vmpc(nc,3);
                glf(nforf2)=glf(nforf2)+pnlty*vmpc(nc,2)*vmpc(nc,3);
                if(nforf1>nforf2)
                    nw=nforf1-nforf2+1;
                    glk(nforf2,nw)=glk(nforf2,nw)+pnlty*vmpc(nc,1)*vmpc(nc,2);
                    glf(nforf1)=vmpc(nc,4);
                else
                    nw=nforf2-nforf1+1;
                    glk(nforf1,nw)=glk(nforf1,nw)+pnlty*vmpc(nc,1)*vmpc(nc,2);
                    glf(nforf2)=vmpc(nc,4);
                end
            end
        end
        %c
        if(nprnt==2)
            %c  print assembled coefficient matrices if required
            fprintf(fid_out,'\nGlobal coefficient matrix, [GLK]:\n'); % write(it,570)
            for i=1:neq
                for j=1:nhbw
                    fprintf(fid_out,'\t% 3.5e',glk(i,j));
                end
                fprintf(fid_out,'\n');
            end
            %         fprintf(fid_out,'%3.5e\n',glk);  %write(it,540) (glk(i,j),j=1:nhbw)
            if(item>=3)
                fprintf(fid_out,'\nGlobal coefficient matrix, [GLM]:\n'); % write(it,575)
                for i=1:neq
                    for j=1:nhbw
                        fprintf(fid_out,'\t% 3.5e',glm(i,j));                           % write(it,540) (glm(i,j),j=1:nhbw)
                    end
                    fprintf(fid_out,'\n');
                end
            else
                fprintf(fid_out,'\nGlobal source vector, {GLF}:\n');
                for i=1:neq
                    fprintf(fid_out,'\t% 3.5e',glf(i));
                end  %write(it,540) (glf(i),i=1:neq)
                fprintf(fid_out,'\n');
            end
        end
        %c
        %c     call subroutine boundary to impose essential, natural and newton's
        %c     type boundary conditions on the primary and secondary variables.
        %c
        boundarysub(neq,nhbw,nspv,nssv,nnbc,ndf,dt,item,alfa,ibdy,ispv,issv,inbc,uref,vspv,vssv,vnbc,gu0,mxebc,mxnbc,mxmbc,mxneq,neqr)
        if(nprnt==2)
            %c     print assembled coefficient matrices if required
            fprintf(fid_out,'\nGlobal coefficient matrix, [GLK]:\n'); % write(it,570)
            for i=1:neq
                for j=1:nhbw
                    fprintf(fid_out,'\t% 3.5e',glk(i,j));
                end
                fprintf(fid_out,'\n');
            end
        end
        %c
        if(item>=3)
            % call MATLAB function eigs (only 6 eigenvalues are printed)
            % to solve for the eigenvalues and eigenvectors
            [egnvec,egnval]=eig(glk,glm,'chol');
            neqr=size(egnvec,1);
            for nvec=1:neqr
                if(egnval(nvec,nvec)>0)
                    frqncy=sqrt(egnval(nvec,nvec));
                    fprintf(fid_out,'\nEIGENVALUE(%d) = % 3.5e \t SQRT(EGNVAL) = % 3.5e\n',nvec,egnval(nvec,nvec),frqncy); % write(it,700)nvec,egnval(nvec),frqncy
                    fprintf(fid_out,'EIGENVECTOR:\n');
                    for nrow=1:neq
                        fprintf(fid_out,'% 3.5e\t', egnvec(nrow,nvec)); % write(it,540)(egnvec(i,nvec),i=1:neqr)
                    end
                    fprintf(fid_out,'\n');
                end
            end
            ianalysis=1;
        else
            %c
            ires = 0; %c  call subroutine eqnsolvr to solve the finite-element equations
            eqnsolvr (neq,nhbw,glk,glf,ires)
            glf=pv_solution;
            %c
            if(item==0)
                fprintf('\n\tSOLUTION (values of pvs) at the NODES:\n'); % write(it,590)
                for ni=1:neq %write(it,540) (glf(ni),ni=1:neq)
                    fprintf('\t\t% 3.5e\n',glf(ni));
                end
                fprintf(fid_out,'\nSOLUTION (values of pvs) at the NODES:\n'); % write(it,590)
                for ni=1:neq %write(it,540) (glf(ni),ni=1:neq)
                    fprintf(fid_out,'% 3.5e ',glf(ni));
                end
                fprintf(fid_out,'\n');
                iprint=1;
            else
                if(nt==0)
                    for i=1:neq
                        gu2(i)=glf(i);
                    end
                    nt=nt+1;
                    time=time+dt;
                    ianalysis=0; %goto 150
                    iprint=0;
                else
                    iprint=1;
                    %c  Compute and print current values of gu0, gu1, and gu2
                    for i=1:neq
                        if(item==2)
                            acclrn=a3*(glf(i)-gu0(i))-a4*gu1(i)-a5*gu2(i);
                            gu1(i)=gu1(i)+a2*gu2(i)+a1*acclrn;
                            gu2(i)=acclrn;
                            gpu(i)=gu0(i);
                        else
                            gpu(i)=gu0(i);
                        end
                        gu0(i)=glf(i);
                    end
                    %c
                    diff=0.0;
                    soln=0.0;
                    for i=1:neq
                        soln=soln+gu0(i)*gu0(i);
                        diff=diff+(glf(i)-gpu(i))^2;
                    end
                    prcnt=sqrt(diff/soln);
                    if(prcnt<=tolrns)
                        fprintf('^* The solution has reached a steady state ^*\n');%write(it,640)
                        fprintf('Solution at the two consecutive time steps follows:')
                        for i=1:neq
                            fprintf('% 3.5e\t',gpu(i)); %write(it,540) (gpu(i),i=1:neq)
                        end
                        for i=1:neq
                            fprintf('% 3.5e\t',gu0(i)); %write(it,540) (gu0(i),i=1:neq)
                        end
                        ianalysis=1; %exit
                    else
                        if(intvl<=0)
                            intvl=1;
                        end
                        nten=(nt/intvl)*intvl;
                        if(nten==nt)
                            fprintf(fid_out,'\nTime = %3.5e time step number = %d\n',time,nt); %write(it,600) time, nt
                            fprintf(fid_out,'\nSOLUTION (values of PVS) at the nodes: \n'); %write(it,590)
                            for i=1:neq
                                fprintf(fid_out,'% 3.5e\t',gu0(i));%write(it,540) (gu0(i),i=1:neq)
                            end
                            fprintf(fid_out,'\n');
                            if(item~=1)
                                if(nprnt<4)
                                    fprintf(fid_out,'\nFIRST TIME DERIVATIVE of the primary variables: \n'); %write(it,645)
                                    for i=1:neq
                                        fprintf(fid_out,'% 3.5e\t',gu1(i));%write(it,540) (gu1(i),i=1:neq)
                                    end
                                    fprintf(fid_out,'\n');
                                    fprintf(fid_out,'\nSECOND TIME DERIVATIVE of the primary variables: \n'); %write(it,646)
                                    for i=1:neq
                                        fprintf(fid_out,'% 3.5e\t',gu2(i));%write(it,540) (gu2(i),i=1:neq)
                                    end
                                    fprintf(fid_out,'\n');
                                end
                            end
                            nt=nt+1;
                            time=time+dt;
                            iprint=1;
                        else
                            nt=nt+1;
                            time=time+dt;
                            ianalysis=0; %go to 150
                        end
                    end
                end
            end
            % -------------------------------------------------------------------------------
            %c             _______________________________________________
            %c            |                                               |
            %c            |     p o s t - p r o c e s s o r   u n i t     |
            %c            |_______________________________________________|
            %c
            % -------------------------------------------------------------------------------
            if(iprint==1)
                if(nmpc==0)
                    if(nprnt<=1)
                        if(model==1)
                            fprintf(fid_out,'______________________________________________________________________________\n\n');
                        else
                            if(model==4)
                                fprintf(fid_out,'\n\t\tGeneralized internal forces in the element\n');
                                fprintf(fid_out,'\tSecond line gives the results in the global coordinates\n\n');
                                %  write(it,630)
                            end
                            fprintf(fid_out,'______________________________________________________________________________\n\n');
                            % write(it,520)
                        end
                        %c
                        if(model==1)
                            fprintf(fid_out,'______________________________________________________________________________\n\n');
                            fprintf(fid_out,'x is the Global coord. if icont=1 and it is the local coord. if icont=0\n');
                            if(ntype==0)
                                fprintf(fid_out,'\t\t x \t\tP. Variable \tS. Variable\n'); %write(it,610)
                                fprintf(fid_out,'______________________________________________________________________________\n\n');
                            else
                                fprintf(fid_out,'\t\t x \t\tDisplacement \t Radial Stress \t Hoop Stress\n');
                                fprintf(fid_out,'______________________________________________________________________________\n\n');
                            end
                        end
                        %c
                        if(model==2)||(model==3)
                            fprintf(fid_out,'______________________________________________________________________________\n\n');
                            fprintf(fid_out,'x is the Global coord. if icont=1 and it is the local coord. if icont=0\n');
                            if(ntype==0)
                                fprintf(fid_out,'\t x \t\t\tDeflect. \t\t rotation \t\t B. Moment \t\t Shear Force \n'); %write(it,650)
                                fprintf(fid_out,'______________________________________________________________________________\n\n');
                            else
                                fprintf(fid_out,'\t x \t\t\tDeflect. \t\t rotation \t\t B. Moment, mr \t\t moment, mt \t\t Shear Force \n'); %write(it,660)
                                fprintf(fid_out,'______________________________________________________________________________\n\n');
                            end
                        end
                        %c
                        if(model==4)
                            if(ntype==0)
                                fprintf(fid_out, 'ele  force, h1   force, v1   force, h2  force, v2 \n');                       %write(it,680)
                                fprintf(fid_out,'______________________________________________________________________________\n\n');
                            else
                                fprintf(fid_out,'ele  force, h1   force, v1  moment, m1  force, h2, force, v2  moment, m2\n');
                                fprintf(fid_out,'______________________________________________________________________________\n\n');
                            end
                        end
                        %c
                        if(model==1)
                            fprintf(fid_out,'______________________________________________________________________________\n\n');
                        else
                            fprintf(fid_out,'______________________________________________________________________________\n\n'); %write(it,520)
                        end
                        %c
                        if(model<=3)
                            postproc(dcax,dcbx,dccx,f3,glf,glx,nod,icont,ielem,npe,model,ntype,item,mxelm,mxneq,mxnod,nem,ndf,fid_out)
                        else
                            for n=1:nem
                                cn1=cs(n);
                                sn1=sn(n);
                                %c   call transfrm to compute element stiffness matrix and force vector
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
                                transfrm(mxelm,n,ntype,pr,se,sl,sa,si,cs,sn,cnt,snt,hf,vf,pf,xb);
                                %c     compute the force and moment resultants
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
                                fprintf(fid_out,'%d \t',n);
                                for i=1:nn %  write(6,150)n, (elf(i),i=1,nn)
                                    fprintf(fid_out,'% 3.5e \t',elf(i));
                                end
                                fprintf(fid_out,'\n');
                                fprintf(fid_out,' \t');
                                for i=1:nn %  write(6,160)   (elr(i),i=1,nn)
                                    fprintf(fid_out,'% 3.5e \t',elr(i));
                                end
                                fprintf(fid_out,'\n\n');
                            end
                        end
                        %c
                        if(model==1)
                            fprintf(fid_out,'______________________________________________________________________________\n\n');
                        else
                            fprintf(fid_out,'______________________________________________________________________________\n\n');
                        end
                    end
                else
                    %c calculate the reactions at the points where constraints are imposed
                    for nc=1:nmpc
                        nforf1=(imc1(nc,1)-1)*ndf+imc1(nc,2);
                        nforf2=(imc2(nc,1)-1)*ndf+imc2(nc,2);
                        gu0(nc)=-pnlty*vmpc(nc,1)*(vmpc(nc,1)*glf(nforf1)+vmpc(nc,2)*glf(nforf2)-vmpc(nc,3));
                        gu1(nc)=-pnlty*vmpc(nc,2)*(vmpc(nc,1)*glf(nforf1)+vmpc(nc,2)*glf(nforf2)-vmpc(nc,3));
                    end
                    fprintf(fid_out,'\nForces at the constrained points:\n\n'); %  write(it,545)
                    for i=1:nmpc
                        fprintf (fid_out,'% 3.5e\t',gu0(i));  %  write(it,540)(gu0(i),i=1:nmpc)
                    end
                    fprintf (fid_out,'\n');
                    for i=1:nmpc
                        fprintf (fid_out,'% 3.5e\t',gu1(i));  %  write(it,540)(gu1(i),i=1:nmpc)
                    end
                    fprintf (fid_out,'\n');
                end
                %c
                if(item==0)
                    fprintf('\n\t***ANALYSIS COMPLETED***\n');
                    ianalysis=1;
                else
                    if(nt>1)
                        if(nt<ntime)
                            if(prcnt>tolrns)
                                ianalysis=0; % goto 150
                            end
                        else
                            ianalysis=1;
                            fprintf('\t*** Number of Time Steps Exceeded NTIME *** \n');
                            fprintf(fid_out,'\t******Number of Time Steps Exceeded NTIME******\n');
                        end
                    end
                end
            end
        end
    end
    fclose(fid_in);
    fclose(fid_out);
end
% c
% c     ----------------------------------------------------------------
% c                       f   o   r   m   a   t   s
% c     ----------------------------------------------------------------
% c
%   300 format(20a4)
%   310 format(8x,'output from program   fem1d   by j n reddy')
%   320 format(/,4x,'^* analysis of model',i2,', and type',i2,
%      *       ' problem ^*',/,15x,'(see the code below)',/,
%      *       /,4x,'model=1:ntype=0: a problem described by model eq. 1',
%      *       /,4x,'model=1:ntype=1: a circular disk (plane stress) ',
%      *       /,4x,'model=1:ntype>1: a circular disk (plane strain) ',
%      *       /,4x,'model=2,ntype=0: a timoshenko beam (rie) problem',
%      *       /,4x,'model=2,ntype=1: a timoshenko plate (rie) problem',
%      *       /,4x,'model=2,ntype=2: a timoshenko beam (cie) problem',
%      *       /,4x,'model=2,ntype>2: a timoshenko plate (cie) problem',
%      *       /,4x,'model=3,ntype=0: a euler-bernoulli beam problem',
%      *       /,4x,'model=3,ntype>0: a euler-bernoulli circular plate',
%      *       /,4x,'model=4,ntype=0: a plane truss problem',
%      *       /,4x,'model=4,ntype=1: a euler-bernoulli frame problem',
%      *       /,4x,'model=4,ntype=2: a timoshenko (cie) frame problem',/)
%   330 format(/,4x,'time-dependent (transient) analysis ',/)
%   340 format(/,4x,'e i g e n v a l u e  a n a l y s i s',/)
%   350 format(/,8x, 'element type (0, hermite,>0, lagrange)..=',i4,/,
%      *         8x, 'no. of deg. of freeform per node, ndf....=',i4,/,
%      *         8x, 'no. of elements in the mesh, nem........=',i4,/,
%      *         8x, 'no. of total forf in the model, neq......=',i4,/,
%      *         8x, 'half bandwidth of matrix [glk], nhbw ...=',i4,/,
%      *         8x, 'no. of specified primary forf, nspv......=',i4,/,
%      *         8x, 'no. of specified secondary forf, nssv....=',i4,/,
%      *         8x, 'no. of specified newton b. c.: nnbc.....=',i4,/,
%      *         8x, 'no. of speci. multi-pt. cond.: nmpc.....=',i4)
%   360 format(/,3x,'element coefficient matrix, [elm]:',/)
%   370 format(/,3x, 'initial conditions on the primary variables:',/)
%   380 format(/,3x, 'initial cond. on time der. of primary variables:',/)
%   390 format(/,8x,'coefficient, ct0........................=',e12.4,/,
%      *         8x,'coefficient, ct1........................=',e12.4,/,
%      *         8x,'parameter, alfa.........................=',e12.4,/,
%      *         8x,'parameter, gama.........................=',e12.4,/,
%      *         8x,'time increment, dt......................=',e12.4,/,
%      *         8x,'no. of time steps, ntime................=',i4,/,
%      *         8x,'time-step interval to print soln., intvl=',i4,/)
%   400 format(/,8x,'coefficient, ct0........................=',e12.4,/,
%      *         8x,'coefficient, ct1........................=',e12.4,/)
%   410 format(/,3x,'global coordinates of the nodes, {glx}:',/)
%   420 format(/,3x,'coefficients of the differential equation:',/)
%   430 format(/,5x,'properties of element =',i3,//,
%      *         8x,'element length, h ....... =',e12.4)
%   440 format(  8x,'ax0 =',e12.4,5x,'ax1 =',e12.4,/,
%      *         8x,'bx0 =',e12.4,5x,'bx1 =',e12.4,/,
%      *         8x,'cx0 =',e12.4,5x,'cx1 =',e12.4,/,
%      *         8x,'fx0 =',e12.4,5x,'fx1 =',e12.4,5x,'fx2 =',e12.4,/)
%   445 format(  8x,'ax0 =',e12.4,5x,'ax1 =',e12.4,/,
%      *         8x,'bx0 =',e12.4,5x,'bx1 =',e12.4,/,
%      *         8x,'cx0 =',e12.4,5x,'cx1 =',e12.4,/)
%   450 format(8x,'the poisson ratio,           pr........ =',e12.4,/,
%      *       8x,'modulus of elasticity,       se........ =',e12.4,/,
%      *       8x,'length of the element,       sl........ =',e12.4,/,
%      *       8x,'area of cross section,       sa........ =',e12.4,/,
%      *       8x,'moment of inertia,           si........ =',e12.4,/,
%      *       8x,'cosine of orientation,       cn........ =',e12.4,/,
%      *       8x,'sine of orientation,         sn........ =',e12.4,/,
%      *       8x,'axial body force (constant), hf........ =',e12.4,/,
%      *       8x,'transverse body force (cnst),vf........ =',e12.4,/,
%      *       8x,'internal point force,        pf........ =',e12.4,/,
%      *       8x,'location of pf from node 1,  xb........ =',e12.4,/,
%      *       8x,'orientation of pf: cosine,   cst....... =',e12.4,/,
%      *       8x,'orientation of pf: sine,     snt....... =',e12.4,/,
%      *       8x,'nodal connectivity:          nod(i,j).. =',2i6,/)
%   460 format(//,3x,'element no. =', i3,/)
%   470 format(8x,'modulus of elasticity,       se........ =',e12.4,/,
%      *       8x,'length of the element,       sl........ =',e12.4,/,
%      *       8x,'area of cross section,       sa........ =',e12.4,/,
%      *       8x,'cosine of orientation,       cn........ =',e12.4,/,
%      *       8x,'sine of orientation,         sn........ =',e12.4,/,
%      *       8x,'axial body force (constant), hf........ =',e12.4,/,
%      *       8x,'nodal connectivity:          nod(i,j).. =',2i6,/)
%   480 format(/,3x, 'boundary information on primary variables:',/)
%   490 format(5x,2i5,2e15.5)
%   495 format(5x,2i5,2x,2i5,/,5x,4e15.5)
%   500 format(/,3x, 'boundary information on secondary variables:',/)
%   510 format(/,3x, 'boundary information on mixed boundary cond.:',/)
%   515 format(/,3x, 'multi-point constraint information:',/)
%   520 format(2x,78('_'),/)
%   530 format(2x,55('_'),/)
%   540 format(2x,5e13.5)
%   545 format(/,3x,'forces at the constrained points:',/)
%   550 format(/,3x,'element coefficient matrix, [elk]:',/)
%   560 format(/,3x,'element source vector, {elf}:',/)
%   570 format(/,3x,'global coefficient matrix, [glk]:',/)
%   575 format(/,3x,'global coefficient matrix, [glm]:',/)
%   580 format(/,3x,'global source vector, {glf}:',/)
%   590 format(/,1x,'solution (values of pvs) at the nodes: ',/)
%   600 format(/,1x,'time =',e12.4,5x,'time step number =',i3,/)
%   610 format(7x,'  x  ',5x, 'p. variable',2x,'s. variable')
%   620 format(7x,'  x  ',3x, 'displacement',2x,'radial stress',2x,
%      *      'hoop stress')
%   630 format(/,15x,'generalized internal forces in the element',/,
%      * 5x,'(second line gives the results in the global coordinates)')
%   640 format(/,3x,'^* the solution has reached a steady state ^*',
%      *       /,3x,'solution at the two consecutive time steps follows:')
%   645 format(/,2x,'first time derivative of the primary variables:',/)
%   646 format(/,2x,'second time derivative of the primary variables:',/)
%   647 format(3x,'x is the global coord. if icont=1 and it is the local',
%      *       ' coord. if icont=0')
%   650 format(7x,'  x  ',6x, 'deflect.',5x,'rotation',5x,'b. moment',
%      *      3x,'shear force')
%   660 format(7x,'  x  ',6x, 'deflect.',5x,'rotation',4x,'moment, mr',
%      *      3x,'moment, mt',3x,'shear force')
%   670 format(3x,  'ele  force, h1   force, v1  moment, m1  force, h2
%      *force, v2  moment, m2')
%   680 format(3x,  'ele  force, h1   force, v1   force, h2  force, v2')
%   690 format(/,5x,'number of rotations taken in jacobi =',i2,/)
%   700 format(/,5x,'eigenvalue(',i2,') = ',e14.6,2x,'sqrt(egnval) = ',
%      *       e13.5,/,5x,'eigenvector:')
%   710 format(/,5x,'^^* number of time steps exceeded ntime ^^*',/)
%       close(in)
%       close(it)
%       stop
% end
    
    
    
    
