SUBROUTINE glob_to_axial(axial,global,coord)
!
! This subroutine transforms the global end reactions
! into an axial force for rod elements (2- or 3-d).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::global(:),coord(:,:)
 REAL(iwp),INTENT(OUT)::axial
 REAL(iwp)::add,ell,zero=0.0_iwp
 INTEGER::ndim,i
 ndim=UBOUND(coord,2)
 add=zero
 DO i=1,ndim
   add=add+(coord(2,i)-coord(1,i))**2
 END DO
 ell=SQRT(add)
 axial=zero
 DO i=1,ndim
   axial=axial+(coord(2,i)-coord(1,i))/ell*global(ndim+i)
 END DO
RETURN
END SUBROUTINE glob_to_axial                       
