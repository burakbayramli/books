SUBROUTINE p111(input_file,output_file)
!-------------------------------------------------------------------------
! Program 11.1 Forced vibration analysis of elastic beams using 2-node
!              beam elements. Consistent mass. Newmark time stepping.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE 
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,j,k,lnode,lsense,ndof=4,nels,neq,nlen,nlf,nn,nod=2,       &
   nodof=2,nof,nlfp,nprops=2,np_types,nr,nstep  
 REAL(iwp)::beta,dtim,fk,fm,f1,f2,gamma,one=1.0_iwp,pt5=0.5_iwp,          &
   zero=0.0_iwp
 CHARACTER(LEN=15)::argv   
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),kdiag(:),lf(:),lp(:),nf(:,:),&
   node(:),num(:),sense(:)   
 REAL(iwp),ALLOCATABLE::a(:),acc(:,:),al(:,:),a1(:),b1(:),cv(:),d(:),     &
   dis(:,:),ell(:),kd(:),km(:,:),kp(:),kv(:),mc(:),mm(:,:),mv(:),         &
   prop(:,:),rl(:),rt(:),v(:),vc(:),vel(:,:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,np_types
 nn=nels+1
 ALLOCATE(nf(nodof,nn),km(ndof,ndof),mm(ndof,ndof),num(nod),g(ndof),      &
   prop(nprops,np_types),ell(nels),g_g(ndof,nels),etype(nels))
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell,dtim,beta,gamma,fm,fk
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(kdiag(neq),a1(0:neq),b1(0:neq),vc(0:neq),kd(0:neq),a(0:neq),    &
   d(0:neq),v(0:neq))
 kdiag=0
!-----------------------loop the elements to find global array sizes------
 elements_1: DO iel=1,nels 
   num=(/iel,iel+1/)
   CALL num_to_g(num,nf,g)
   g_g(:,iel)=g 
   CALL fkdiag(kdiag,g)
 END DO elements_1
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)),cv(kdiag(neq)),mv(kdiag(neq)),mc(kdiag(neq)),    &
   kp(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
 kv=zero
 mv=zero
!-----------------------global stiffness and mass matrix assembly---------
 elements_2: DO iel=1,nels
   CALL beam_km(km,prop(1,etype(iel)),ell(iel))
   CALL beam_mm(mm,prop(2,etype(iel)),ell(iel))
   g=g_g(:,iel) 
   CALL fsparv(kv,km,g,kdiag)
   CALL fsparv(mv,mm,g,kdiag)
 END DO elements_2
 mc=mv
!-----------------------initial conditions, and load functions------------
 d=zero
 v=zero   !  alternatively READ(10,*)d(1:),v(1:)
 READ(10,*)nlf
 ALLOCATE(lf(nlf))    
 DO k=1,nlf
   READ(10,*)lnode,lsense,nlfp
   ALLOCATE(rt(nlfp),rl(nlfp))
   lf(k)=nf(lsense,lnode)
   READ(10,*)(rt(j),rl(j),j=1,nlfp)
   IF(k==1)THEN
     nstep=NINT((rt(nlfp)-rt(1))/dtim)+1
     ALLOCATE(al(nstep,nlf))
   END IF
   CALL interp(k,dtim,rt,rl,al,nstep)
   DEALLOCATE(rt,rl)
 END DO
 f1=beta*dtim**2
 f2=beta*dtim
 cv=fm*mv+fk*kv
 kp=mv/f1+gamma*cv/f2+kv
 CALL sparin(mc,kdiag)
 CALL sparin(kp,kdiag)
 a=zero
 a(lf(:))=al(1,:)
 CALL linmul_sky(cv,v,vc,kdiag)
 CALL linmul_sky(kv,d,kd,kdiag)
 a=a-vc-kd
 CALL spabac(mc,a,kdiag)
 READ(10,*)nof
 ALLOCATE(node(nof),sense(nof),lp(nof),dis(nstep,nof),vel(nstep,nof),     &
   acc(nstep,nof))
 READ(10,*)(node(i),sense(i),i=1,nof)
 DO i=1,nof
   lp(i)=nf(sense(i),node(i))
 END DO
 dis(1,:)=d(lp)
 vel(1,:)=v(lp)
 acc(1,:)=a(lp) 
!-----------------------time stepping loop--------------------------------
 DO j=2,nstep
   a1=d/f1+v/f2+a*(pt5/beta-one)
   b1=gamma*d/f2-v*(one-gamma/beta)-dtim*a*(one-pt5*gamma/beta)
   CALL linmul_sky(mv,a1,vc,kdiag)
   CALL linmul_sky(cv,b1,kd,kdiag)
   d=vc+kd
   d(lf(:))=d(lf(:))+al(j,:)
   CALL spabac(kp,d,kdiag)  
   v=gamma*d/f2-b1
   a=d/f1-a1
   dis(j,:)=d(lp)
   vel(j,:)=v(lp)
   acc(j,:)=a(lp)
 END DO
 DO i=1,nof
   WRITE(11,'(/,2(A,I3))')" Output at node",node(i),", sense",sense(i)
   WRITE(11,'(A)')"    time        disp        velo        accel"
   DO j=1,nstep
   WRITE(11,'(4E12.4)')(j-1)*dtim,dis(j,i),vel(j,i),acc(j,i)
   END DO
 END DO   
END SUBROUTINE p111
