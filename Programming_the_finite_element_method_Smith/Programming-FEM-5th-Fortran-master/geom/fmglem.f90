SUBROUTINE fmglem(fnxe,fnye,lnxe,g_num,lifts)
! Used in p69
! This subroutine returns g_num for the mesh.
!
 IMPLICIT NONE
 INTEGER,INTENT(IN)::fnxe,fnye,lnxe,lifts
 ! output: g_num matrix
 INTEGER,INTENT(OUT)::g_num(:,:)
 INTEGER::ig,i,j,ii,ilast       
 ig=0
 DO i=1,fnye
   DO j=1,fnxe
     ig=ig+1
     g_num(1,ig)=(j*2-1)+(i-1)*(fnxe+1)+(i-1)*(2*fnxe+1)
     g_num(8,ig)=g_num(1,ig)+1
     g_num(7,ig)=g_num(1,ig)+2
     g_num(2,ig)=i*(2*fnxe+1)+j+(i-1)*(fnxe+1)
     g_num(6,ig)=g_num(2,ig)+1
     g_num(3,ig)=i*(2*fnxe+1)+i*(fnxe+1)+(j*2-1)
     g_num(4,ig)=g_num(3,ig)+1
     g_num(5,ig)=g_num(3,ig)+2
   END DO
 END DO
 ilast=g_num(5,ig)
 DO ii=1,lifts-1
   DO j=1,lnxe-(ii-1)*1
     ig=ig+1
     g_num(1,ig)=ilast-(lnxe-ii+1)*2+(j-1)*2
     g_num(8,ig)=g_num(1,ig)+1
     g_num(7,ig)=g_num(1,ig)+2
     g_num(2,ig)=ilast+j
     g_num(6,ig)=g_num(2,ig)+1
     g_num(3,ig)=ilast+(lnxe-ii+1)+1+(j*2-1)
     g_num(4,ig)=g_num(3,ig)+1
     g_num(5,ig)=g_num(3,ig)+2
   END DO
   ilast=g_num(5,ig)
 END DO
RETURN
END SUBROUTINE fmglem
