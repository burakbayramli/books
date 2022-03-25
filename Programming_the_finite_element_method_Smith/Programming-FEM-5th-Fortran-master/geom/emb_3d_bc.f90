SUBROUTINE emb_3d_bc(ifix,nx1,nx2,ny1,ny2,nze,nf)
! Used in p612, p613
! This subroutine generates the nf array for a 3-d slope geometry.
! Side boundary conditions:
! smooth-smooth  ifix=1  (for checking against plane-strain)
! rough -smooth  ifix=2  (for centerline symmetry)
! rough -rough   ifix=3
!
 IMPLICIT NONE
 INTEGER,INTENT(IN)::nx1,nx2,ny1,ny2,nze,ifix
 INTEGER,INTENT(OUT)::nf(:,:)
 INTEGER::nm,ic,i,j,k,nye
 nye=ny1+ny2
 nm=0
 ic=0
!
!                       boundary condition for the front face
!
 DO i=1,2*nye
   nm=nm+1
   nf(:,nm)=0
   IF(ifix==1)THEN
     ic=ic+1
     nf(2,nm)=ic
   END IF
 END DO
 nm=nm+1
 nf(:,nm)=0
 DO j=1,nx1
   DO i=1,nye
     nm=nm+1
     nf(:,nm)=0
     IF(ifix==1)THEN
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic
     END IF
   END DO
   nm=nm+1
   nf(:,nm)=0
   DO i=1,2*nye
     nm=nm+1
     nf(:,nm)=0
     IF(ifix==1)THEN
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic
     END IF
   END DO
   nm=nm+1
   nf(:,nm)=0
 END DO
 DO j=1,nx2
   DO i=1,ny2
     nm=nm+1
     nf(:,nm)=0
     IF(ifix==1)THEN
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic
     END IF
   END DO
   nm=nm+1
   nf(:,nm)=0
   DO i=1,2*ny2
     nm=nm+1
     nf(:,nm)=0
     IF(ifix==1)THEN
       IF(j<nx2)THEN
         ic=ic+1
         nf(1,nm)=ic
       END IF 
       ic=ic+1
       nf(2,nm)=ic
     END IF
   END DO
   nm=nm+1
   nf(:,nm)=0
 END DO    
!
!                       boundary conditions for the middle layer
!
 DO i=1,nye
   nm=nm+1
   nf(1,nm)=0
   ic=ic+1
   nf(3,nm)=ic
   ic=ic+1
   nf(2,nm)=ic
 END DO
 nm=nm+1
 nf(:,nm)=0
 DO j=1,nx1
   DO i=1,nye
     nm=nm+1
     ic=ic+1
     nf(1,nm)=ic
     ic=ic+1
     nf(3,nm)=ic
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(:,nm)=0
 END DO
 DO j=1,nx2
   DO i=1,ny2
     nm=nm+1
     IF(j==nx2)THEN
       nf(1,nm)=0
     ELSE
       ic=ic+1
       nf(1,nm)=ic
     END IF
     ic=ic+1
     nf(3,nm)=ic
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(:,nm)=0
 END DO
!all other layers
DO k=1,nze-1
  DO i=1,2*nye
    nm=nm+1
    nf(1,nm)=0
    ic=ic+1
    nf(3,nm)=ic
    ic=ic+1
    nf(2,nm)=ic
  END DO
  nm=nm+1
  nf(:,nm)=0
  DO j=1,nx1
    DO i=1,nye
      nm=nm+1
      ic=ic+1
      nf(1,nm)=ic
      ic=ic+1
      nf(3,nm)=ic
      ic=ic+1
      nf(2,nm)=ic
    END DO
    nm=nm+1
    nf(:,nm)=0
    DO i=1,2*nye
      nm=nm+1
      ic=ic+1
      nf(1,nm)=ic
      ic=ic+1
      nf(3,nm)=ic
      ic=ic+1
      nf(2,nm)=ic
    END DO
    nm=nm+1
    nf(:,nm)=0
  END DO
  DO j=1,nx2
    DO i=1,ny2
      nm=nm+1
      ic=ic+1
      nf(1,nm)=ic
      ic=ic+1
      nf(3,nm)=ic
      ic=ic+1
      nf(2,nm)=ic
    END DO
    nm=nm+1
    nf(:,nm)=0
    DO i=1,2*ny2
      nm=nm+1
      IF(j==nx2)THEN
        nf(1,nm)=0
      ELSE
        ic=ic+1
        nf(1,nm)=ic
      END IF
      ic=ic+1
      nf(3,nm)=ic
      ic=ic+1
      nf(2,nm)=ic
    END DO
    nm=nm+1
    nf(:,nm)=0
  END DO
! middle layers
  DO i=1,nye
    nm=nm+1
    nf(1,nm)=0
    ic=ic+1
    nf(3,nm)=ic
    ic=ic+1
    nf(2,nm)=ic
  END DO
  nm=nm+1
  nf(:,nm)=0
  DO j=1,nx1
    DO i=1,nye
      nm=nm+1
      ic=ic+1
      nf(1,nm)=ic
      ic=ic+1
      nf(3,nm)=ic
      ic=ic+1
      nf(2,nm)=ic
    END DO
    nm=nm+1
    nf(:,nm)=0
  END DO
  DO j=1,nx2
    DO i=1,ny2
      nm=nm+1
      IF(j==nx2)THEN
        nf(1,nm)=0
      ELSE
        ic=ic+1
        nf(1,nm)=ic
      END IF
      ic=ic+1
      nf(3,nm)=ic
      ic=ic+1
      nf(2,nm)=ic
    END DO
    nm=nm+1
    nf(:,nm)=0
  END DO
END DO
!
!                       boundary conditions for back face
!
 DO i=1,2*nye
   nm=nm+1
   nf(:,nm)=0
   IF(ifix==1.OR.ifix==2)THEN
     ic=ic+1
     nf(2,nm)=ic
   END IF
 END DO
 nm=nm+1
 nf(:,nm)=0
 DO j=1,nx1
   DO i=1,nye
     nm=nm+1
     nf(:,nm)=0
     IF(ifix==1.OR.ifix==2)THEN
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic
     END IF
   END DO
   nm=nm+1
   nf(:,nm)=0
   DO i=1,2*nye
     nm=nm+1
     nf(:,nm)=0
     IF(ifix==1.OR.ifix==2)THEN
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic
     END IF
   END DO
   nm=nm+1
   nf(:,nm)=0
 END DO
 DO j=1,nx2
   DO i=1,ny2
     nm=nm+1
     nf(:,nm)=0
     IF(ifix==1.OR.ifix==2)THEN
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic
     END IF
   END DO
   nm=nm+1
   nf(:,nm)=0
   DO i=1,2*ny2
     nm=nm+1
     nf(:,nm)=0
     IF(ifix==1.OR.ifix==2)THEN
       IF(j<nx2)THEN
         ic=ic+1
         nf(1,nm)=ic
       END IF 
       ic=ic+1
       nf(2,nm)=ic
     END IF
   END DO
   nm=nm+1
   nf(:,nm)=0
 END DO    
RETURN
END SUBROUTINE emb_3d_bc