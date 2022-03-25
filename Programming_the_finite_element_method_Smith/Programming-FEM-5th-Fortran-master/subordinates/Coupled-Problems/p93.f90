SUBROUTINE p93(input_file,output_file)
!-------------------------------------------------------------------------
! Program 9.3: One dimensional coupled consolidation analysis of a Biot
!              poro-elastic solid using 2-node "line" elements.
!              Freedoms numbered in order v-uw.
!-------------------------------------------------------------------------
 USE main
 USE geom
 IMPLICIT NONE
 CHARACTER(len=60),INTENT(IN) :: input_file
 CHARACTER(len=60),INTENT(OUT) :: output_file
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,iel,j,k,loaded_nodes,ndof=4,nels,neq,nlen,nlfp,nls,nn,nod=2,  &
   nodof=2,npri,nprops=2,np_types,nr,nres,nstep,ntime
 REAL(iwp)::at,a0,dtim,hme,one=1.0_iwp,pt5=0.5_iwp,settl,theta,time,      &
   uav,uavs,zero=0.0_iwp
 CHARACTER(LEN=15)::argv
!-----------------------dynamic arrays------------------------------------
 INTEGER,ALLOCATABLE::etype(:),g(:),g_g(:,:),kdiag(:),nf(:,:),no(:),num(:)
 REAL(iwp),ALLOCATABLE::al(:),ans(:),c(:,:),ell(:),kc(:,:),ke(:,:),       &
   km(:,:),kv(:),lf(:,:),loads(:),phi0(:),phi1(:),press(:),prop(:,:),     &
   store_kc(:,:,:),val(:)
!-----------------------input and initialisation--------------------------
 OPEN(10,FILE=input_file)
 OPEN(11,FILE=output_file)
 READ(10,*)nels,np_types
 nn=nels+1
 ALLOCATE(num(nod),etype(nels),kc(nod,nod),prop(nprops,np_types),         &
   ell(nels),nf(nodof,nn),g(ndof),g_g(ndof,nels),km(nod,nod),             &
   store_kc(nod,nod,nels),ke(ndof,ndof),phi0(nodof),phi1(nodof),c(nod,nod))
!---prop(1,:)=permeability (k/gamma_w), prop(2,:)=compressibility (m_v)----
 READ(10,*)prop
 etype=1
 IF(np_types>1)READ(10,*)etype
 READ(10,*)ell
 READ(10,*)dtim,nstep,theta,npri,nres,ntime
 nf=1
 READ(10,*)nr,(k,nf(:,k),i=1,nr)
 CALL formnf(nf)
 neq=MAXVAL(nf)
 ALLOCATE(kdiag(neq),loads(0:neq),ans(0:neq),press(nn))
 READ(10,*)loaded_nodes
 ALLOCATE(no(loaded_nodes),val(loaded_nodes))
 READ(10,*)(no(i),val(i),i=1,loaded_nodes)
 READ(10,*)nlfp
 ALLOCATE(lf(2,nlfp))
 READ(10,*)lf
 nls=FLOOR(lf(1,nlfp)/dtim)
 IF(nstep>nls)nstep=nls
 ALLOCATE(al(nstep))
 CALL load_function(lf,dtim,al)
 kdiag=0
!-----------------------loop the elements to find global arrays sizes-----
 elements_1: DO iel=1,nels
   num=(/iel,iel+1/)
   g(1:2)=nf(1,num)
   g(3:4)=nf(2,num)
   g_g(:,iel)=g
   CALL fkdiag(kdiag,g)
 END DO elements_1
 DO i=2,neq
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(neq)))
 WRITE(11,'(2(A,I5))')                                                    &
   " There are",neq," equations and the skyline storage is",kdiag(neq)
!-----------------------global conductivity and "mass" matrix assembly----
 kv=zero
 c(:,1)=-pt5
 c(:,2)= pt5
 elements_2: DO iel=1,nels
   g=g_g(:,iel)
   CALL rod_km(kc,prop(1,etype(iel)),ell(iel))
   kc=kc*dtim
   CALL rod_km(km,one/prop(2,etype(iel)),ell(iel))
   store_kc(:,:,iel)=kc
   CALL formke(km,kc,c,ke,theta)
   CALL fsparv(kv,ke,g,kdiag)
 END DO elements_2
 CALL sparin_gauss(kv,kdiag) !---factorise equations----
 WRITE(11,'(/2A,I4,A)')"    Time          Uav         Uavs",              &
   "    Settle_top  Pressure (node",nres,")"
 loads=zero
 hme=zero 
 DO iel=1,nels
   hme=hme+ell(iel)*prop(2,etype(iel))
 END DO 
 WRITE(11,'(5E12.4)')0.0,0.0,0.0,0.0,loads(nf(2,nres))
!-----------------------time stepping loop--------------------------------
 time_steps: DO j=1,nstep
   time=j*dtim
   ans=zero
   a0=zero
   settl=SUM(al(1:j))*hme
   elements_3: DO iel=1,nels
   g=g_g(:,iel)
   kc=store_kc(:,:,iel)
   phi0=loads(g(nodof+1:))
   phi1=MATMUL(kc,phi0)
   ans(g(nodof+1:))=ans(g(nodof+1:))+phi1
 END DO elements_3
!-----------------------apply loading increment---------------------------
   ans(nf(1,no))=val*al(j)
!-----------------------equation solution---------------------------------
   CALL spabac_gauss(kv,ans,kdiag)
   loads=loads+ans
   loads(0)=zero
   at=zero
   DO iel=1,nels
     at=at+pt5*ell(iel)*abs(loads(nf(2,iel))+loads(nf(2,iel+1)))
     a0=a0+ell(iel)*SUM(al(1:j))
   END DO
   uav=(a0-at)/a0
   uavs=loads(nf(1,1))/settl
   IF(j==ntime)press(:)=loads(nf(2,:))
   IF(j/npri*npri==j)WRITE(11,'(5E12.4)')time,uav,uavs,loads(nf(1,1)),    &
     loads(nf(2,nres))
 END DO time_steps
 WRITE(11,'(/A,E10.4,A)')"    Depth     Pressure (time=",ntime*dtim,")"
 WRITE(11,'(2E12.4)')zero,zero
 WRITE(11,'(2E12.4)')(SUM(ell(1:i)),press(i+1),i=1,nels)

END SUBROUTINE p93
