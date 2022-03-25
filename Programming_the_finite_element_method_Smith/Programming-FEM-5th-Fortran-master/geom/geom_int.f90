MODULE geom
!'OOP' in Fortran
INTERFACE
SUBROUTINE bc_rect(nxe,nye,nf,dir)
 IMPLICIT NONE
 INTEGER,INTENT(IN)::nxe,nye
 CHARACTER(LEN=1),INTENT(IN)::dir
 INTEGER,INTENT(OUT)::nf(:,:)
END SUBROUTINE bc_rect
!
SUBROUTINE emb_2d_bc(nx1,nx2,ny1,ny2,nf)
 IMPLICIT NONE
 INTEGER,INTENT(IN)::nx1,nx2,ny1,ny2
 INTEGER,INTENT(OUT)::nf(:,:)
END SUBROUTINE emb_2d_bc
!
SUBROUTINE emb_2d_geom(iel,nx1,nx2,ny1,ny2,w1,s1,w2,h1,h2,coord,num)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::w1,s1,w2,h1,h2
 INTEGER,INTENT(IN)::iel,nx1,nx2,ny1,ny2
 REAL(iwp),INTENT(OUT)::coord(:,:)
 INTEGER,INTENT(OUT)::num(:)
END SUBROUTINE emb_2d_geom
!
SUBROUTINE emb_3d_bc(ifix,nx1,nx2,ny1,ny2,nze,nf)
 IMPLICIT NONE
 INTEGER,INTENT(IN)::nx1,nx2,ny1,ny2,nze,ifix
 INTEGER,INTENT(OUT)::nf(:,:)
END SUBROUTINE emb_3d_bc
!
SUBROUTINE emb_3d_geom(iel,nx1,nx2,ny1,ny2,nze,w1,s1,w2,h1,h2,d1,coord,num)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::w1,s1,w2,h1,h2,d1
 INTEGER,INTENT(IN)::iel,nx1,nx2,ny1,ny2,nze
 REAL(iwp),INTENT(OUT)::coord(:,:)
 INTEGER,INTENT(OUT)::num(:)
END SUBROUTINE emb_3d_geom
!
SUBROUTINE fmcoem(g_num,g_coord,fwidth,fdepth,width,depth,                &
  lnxe,lifts,fnxe,fnye,itype)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::g_num(:,:),lnxe,lifts,fnxe,fnye,itype
 REAL(iwp),INTENT(IN)::fwidth(:),fdepth(:),width(:),depth(:)
 REAL(iwp),INTENT(OUT)::g_coord(:,:)
END SUBROUTINE fmcoem                       
!
SUBROUTINE fmglem(fnxe,fnye,lnxe,g_num,lifts)
 IMPLICIT NONE
 INTEGER,INTENT(IN)::fnxe,fnye,lnxe,lifts
 INTEGER,INTENT(OUT)::g_num(:,:)
END SUBROUTINE fmglem
!
SUBROUTINE formnf(nf)
 IMPLICIT NONE
 INTEGER,INTENT(IN OUT)::nf(:,:)
END SUBROUTINE formnf 
!
SUBROUTINE geom_freesurf(iel,nxe,fixed_seep,fixed_down,down,              &
  width,angs,surf,coord,num)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::width(:),angs(:),surf(:),down 
 REAL(iwp),INTENT(OUT)::coord(:,:)
 INTEGER,INTENT(IN)::iel,nxe,fixed_seep,fixed_down
 INTEGER,INTENT(OUT)::num(:)
END SUBROUTINE geom_freesurf                                               
!
SUBROUTINE geom_rect(element,iel,x_coords,y_coords,coord,num,dir)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x_coords(:),y_coords(:) 
 REAL(iwp),INTENT(OUT)::coord(:,:)
 CHARACTER(LEN=15),INTENT(IN)::element
 CHARACTER(LEN=1),INTENT(IN)::dir
 INTEGER,INTENT(IN)::iel
 INTEGER,INTENT(OUT)::num(:)
END SUBROUTINE geom_rect
!
SUBROUTINE hexahedron_xz(iel,x_coords,y_coords,z_coords,coord,num)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::iel
 REAL(iwp),INTENT(IN)::x_coords(:),y_coords(:),z_coords(:)
 REAL(iwp),INTENT(OUT)::coord(:,:)
 INTEGER,INTENT(OUT)::num(:)
END SUBROUTINE hexahedron_xz
!
SUBROUTINE mesh_size(element,nod,nels,nn,nxe,nye,nze)
 IMPLICIT NONE               
 CHARACTER(LEN=15),INTENT(IN)::element
 INTEGER,INTENT(IN)::nod,nxe,nye
 INTEGER,INTENT(IN),OPTIONAL::nze
 INTEGER,INTENT(OUT)::nels,nn
END SUBROUTINE mesh_size
!
END INTERFACE
!
END MODULE geom
