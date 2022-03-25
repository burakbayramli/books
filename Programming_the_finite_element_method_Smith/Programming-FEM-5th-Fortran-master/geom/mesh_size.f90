SUBROUTINE mesh_size(element,nod,nels,nn,nxe,nye,nze)
!  Used in many subroutines
!  This subroutine returns the number of elements (nels) and the number
!  of nodes (nn) in a 2-d geometry-created mesh.    
!
 IMPLICIT NONE
 !element: [string] triangle quadrilateral hexahedron
 CHARACTER(LEN=15),INTENT(IN)::element
 !  nod  : [integer] number of nodes per element
 !  nie  : [integer] elements counting in i-direction
 INTEGER,INTENT(IN)::nod,nxe,nye
 INTEGER,INTENT(IN),OPTIONAL::nze
 !  nels : [integer] total number of elements           
 !  nn   : [integer] total number of nodes in problem   
 INTEGER,INTENT(OUT)::nels,nn
 IF(element=="triangle")THEN
   nels=nxe*nye*2
   IF(nod==3)nn=(nxe+1)*(nye+1)
   IF(nod==6)nn=(2*nxe+1)*(2*nye+1)
   IF(nod==10)nn=(3*nxe+1)*(3*nye+1)
   IF(nod==15)nn=(4*nxe+1)*(4*nye+1)
 ELSE IF(element=="quadrilateral")THEN
   nels=nxe*nye
   IF(nod==4)nn=(nxe+1)*(nye+1)
   IF(nod==5)nn=(nxe+1)*(nye+1)+nxe*nye
   IF(nod==8)nn=(2*nxe+1)*(nye+1)+(nxe+1)*nye
   IF(nod==9)nn=(2*nxe+1)*(2*nye+1)
 ELSE IF(element=="hexahedron")THEN
   nels=nxe*nye*nze
   IF(nod==8)nn=(nxe+1)*(nye+1)*(nze+1)
   IF(nod==14)nn=4*nxe*nye*nze+2*(nxe*nye+nye*nze+nze*nxe)+nxe+nye+nze+1
   IF(nod==20)nn=((2*nxe+1)*(nze+1)+(nxe+1)*nze)*(nye+1)+                 &
     (nxe+1)*(nze+1)*nye
 END IF
RETURN
END SUBROUTINE mesh_size
