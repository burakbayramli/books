SUBROUTINE emb_2d_bc(nx1,nx2,ny1,ny2,nf)
! Used in p64
! This subroutine generates the nf array for a 2-d slope geometry.
!
 IMPLICIT NONE
 !nx1 : number of columns of elements in embankment
 !nx2 : number of columns of elements to right of toe
 !ny1 : number of rows of elements in embankment
 !ny2 : number of rows of elements in foundation
 INTEGER,INTENT(IN)::nx1,nx2,ny1,ny2
 !nf  : node freedom array
 INTEGER,INTENT(OUT)::nf(:,:)
 !nye : number of rows of elements in y-direction
 INTEGER::nm,ic,i,j,nye
 nye=ny1+ny2
 nm=0
 ic=0
 DO i=1,2*nye
   nm=nm+1
   nf(1,nm)=0
   ic=ic+1
   nf(2,nm)=ic
 END DO
 nm=nm+1
 nf(1,nm)=0
 nf(2,nm)=0
 DO j=1,nx1-1
   DO i=1,nye
     nm=nm+1
     ic=ic+1
     nf(1,nm)=ic
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(1,nm)=0
   nf(2,nm)=0
   DO i=1,2*nye
     nm=nm+1
     ic=ic+1
     nf(1,nm)=ic
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(1,nm)=0
   nf(2,nm)=0
 END DO
   DO i=1,nye
     nm=nm+1
     ic=ic+1
     nf(1,nm)=ic
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(1,nm)=0
   nf(2,nm)=0
   DO i=1,2*ny1
     nm=nm+1
     ic=ic+1
     nf(1,nm)=ic
     ic=ic+1
     nf(2,nm)=ic
   END DO 
   IF(nx2==0)THEN
   DO i=1,2*ny2
     nm=nm+1
     nf(1,nm)=0
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(1,nm)=0
   nf(2,nm)=0
 ELSE
   DO i=1,2*ny2
     nm=nm+1
     ic=ic+1
     nf(1,nm)=ic
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(1,nm)=0
   nf(2,nm)=0
   DO j=1,nx2
     DO i=1,ny2
       nm=nm+1
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic
     END DO
     nm=nm+1
     nf(1,nm)=0
     nf(2,nm)=0
     DO i=1,2*ny2
       nm=nm+1
       IF(j==nx2)THEN
         nf(1,nm)=0
       ELSE
         ic=ic+1
         nf(1,nm)=ic
       END IF
       ic=ic+1
       nf(2,nm)=ic
     END DO
     nm=nm+1
     nf(1,nm)=0
     nf(2,nm)=0
   END DO
 END IF
RETURN
END SUBROUTINE emb_2d_bc