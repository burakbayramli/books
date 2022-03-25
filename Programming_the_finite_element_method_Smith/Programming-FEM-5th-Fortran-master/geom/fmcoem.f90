SUBROUTINE fmcoem(g_num,g_coord,fwidth,fdepth,width,depth,                &
  lnxe,lifts,fnxe,fnye,itype)
! Used in p69
! This subroutine returns g_coord for the mesh.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::g_num(:,:),lnxe,lifts,fnxe,fnye,itype
 REAL(iwp),INTENT(IN)::fwidth(:),fdepth(:),width(:),depth(:)
 REAL(iwp),INTENT(OUT)::g_coord(:,:)
 INTEGER::ig,i,j,ii,lnye
 REAL(iwp)::pt5=0.5_iwp
 lnye=1
 ig=0
 DO i=1,fnye
   DO j=1,fnxe
     ig=ig+1
     g_coord(1,g_num(1,ig))=fwidth(j)
     g_coord(1,g_num(2,ig))=fwidth(j)
     g_coord(1,g_num(3,ig))=fwidth(j)
     g_coord(1,g_num(5,ig))=fwidth(j+1)
     g_coord(1,g_num(6,ig))=fwidth(j+1)
     g_coord(1,g_num(7,ig))=fwidth(j+1)
     g_coord(1,g_num(4,ig))=                                              &
       pt5*(g_coord(1,g_num(3,ig))+g_coord(1,g_num(5,ig)))
     g_coord(1,g_num(8,ig))=                                              &
       pt5*(g_coord(1,g_num(1,ig))+g_coord(1,g_num(7,ig)))
     g_coord(2,g_num(1,ig))=fdepth(i)
     g_coord(2,g_num(8,ig))=fdepth(i)
     g_coord(2,g_num(7,ig))=fdepth(i)
     g_coord(2,g_num(3,ig))=fdepth(i+1)
     g_coord(2,g_num(4,ig))=fdepth(i+1)
     g_coord(2,g_num(5,ig))=fdepth(i+1)
     g_coord(2,g_num(2,ig))=                                              &
       pt5*(g_coord(2,g_num(1,ig))+g_coord(2,g_num(3,ig)))
     g_coord(2,g_num(6,ig))=                                              &
       pt5*(g_coord(2,g_num(7,ig))+g_coord(2,g_num(5,ig)))
   END DO
 END DO
 IF(itype==1)THEN
   DO ii=1,lifts-1
     DO i=1,lnye
       DO j=1,lnxe-(ii-1)*1
         ig=ig+1
         IF(j==1)THEN
           g_coord(1,g_num(1,ig))=width((ii-1)+j)
           g_coord(1,g_num(5,ig))=width((ii-1)+j+1)
           g_coord(1,g_num(6,ig))=g_coord(1,g_num(5,ig))
           g_coord(1,g_num(7,ig))=g_coord(1,g_num(5,ig))
           g_coord(1,g_num(3,ig))=                                        &
             pt5*(g_coord(1,g_num(1,ig))+g_coord(1,g_num(5,ig)))
           g_coord(1,g_num(2,ig))=                                        &
             pt5*(g_coord(1,g_num(3,ig))+g_coord(1,g_num(1,ig)))
           g_coord(1,g_num(4,ig))=                                        &
             pt5*(g_coord(1,g_num(3,ig))+g_coord(1,g_num(5,ig)))
           g_coord(1,g_num(8,ig))=                                        &
             pt5*(g_coord(1,g_num(1,ig))+g_coord(1,g_num(7,ig)))
           g_coord(2,g_num(1,ig))=depth((ii-1)+i)
           g_coord(2,g_num(8,ig))=depth((ii-1)+i)
           g_coord(2,g_num(7,ig))=depth((ii-1)+i)
           g_coord(2,g_num(5,ig))=depth((ii-1)+i+1)
           g_coord(2,g_num(3,ig))=                                        &
             pt5*(g_coord(2,g_num(1,ig))+g_coord(2,g_num(5,ig)))
           g_coord(2,g_num(2,ig))=                                        &
             pt5*(g_coord(2,g_num(1,ig))+g_coord(2,g_num(3,ig)))
           g_coord(2,g_num(4,ig))=                                        &
             pt5*(g_coord(2,g_num(3,ig))+g_coord(2,g_num(5,ig)))
           g_coord(2,g_num(6,ig))=                                        &
             pt5*(g_coord(2,g_num(5,ig))+g_coord(2,g_num(7,ig)))
         ELSE
           g_coord(1,g_num(1,ig))=width((ii-1)+j)
           g_coord(1,g_num(2,ig))=width((ii-1)+j)
           g_coord(1,g_num(3,ig))=width((ii-1)+j)
           g_coord(1,g_num(5,ig))=width((ii-1)+j+1)
           g_coord(1,g_num(6,ig))=width((ii-1)+j+1)
           g_coord(1,g_num(7,ig))=width((ii-1)+j+1)
           g_coord(1,g_num(4,ig))=                                        &
             pt5*(g_coord(1,g_num(3,ig))+g_coord(1,g_num(5,ig)))
           g_coord(1,g_num(8,ig))=                                        &
             pt5*(g_coord(1,g_num(1,ig))+g_coord(1,g_num(7,ig)))
           g_coord(2,g_num(1,ig))=depth((ii-1)+i)
           g_coord(2,g_num(8,ig))=depth((ii-1)+i)
           g_coord(2,g_num(7,ig))=depth((ii-1)+i)
           g_coord(2,g_num(3,ig))=depth((ii-1)+i+1)
           g_coord(2,g_num(4,ig))=depth((ii-1)+i+1)
           g_coord(2,g_num(5,ig))=depth((ii-1)+i+1)
           g_coord(2,g_num(2,ig))=                                        &
             pt5*(g_coord(2,g_num(1,ig))+g_coord(2,g_num(3,ig)))
           g_coord(2,g_num(6,ig))=                                        &
             pt5*(g_coord(2,g_num(5,ig))+g_coord(2,g_num(7,ig)))
         END IF
       END DO
     END DO
   END DO
 ELSE
   DO ii=1,lifts-1
     DO i=1,lnye
       DO j=1,lnxe-(ii-1)*1
         ig=ig+1
         if(j==1)then
           g_coord(1,g_num(1,ig))=width((ii-1)+j)
           g_coord(1,g_num(5,ig))=width((ii-1)+j+1)
           g_coord(1,g_num(6,ig))=g_coord(1,g_num(5,ig))
           g_coord(1,g_num(7,ig))=g_coord(1,g_num(5,ig))
           g_coord(1,g_num(3,ig))=g_coord(1,g_num(5,ig))
           g_coord(1,g_num(4,ig))=g_coord(1,g_num(5,ig))
           g_coord(1,g_num(8,ig))=                                        &
             pt5*(g_coord(1,g_num(1,ig))+g_coord(1,g_num(7,ig)))
           g_coord(1,g_num(2,ig))=                                        &
             pt5*(g_coord(1,g_num(1,ig))+g_coord(1,g_num(5,ig)))
           g_coord(2,g_num(1,ig))=depth((ii-1)+i)
           g_coord(2,g_num(8,ig))=depth((ii-1)+i)
           g_coord(2,g_num(7,ig))=depth((ii-1)+i)
           g_coord(2,g_num(5,ig))=depth((ii-1)+i+1)
           g_coord(2,g_num(2,ig))=                                        &
             pt5*(g_coord(2,g_num(1,ig))+g_coord(2,g_num(5,ig)))
           g_coord(2,g_num(6,ig))=                                        &
             pt5*(g_coord(2,g_num(5,ig))+g_coord(2,g_num(7,ig)))
           g_coord(2,g_num(3,ig))=g_coord(2,g_num(5,ig))
           g_coord(2,g_num(4,ig))=g_coord(2,g_num(5,ig))
         ELSE
           g_coord(1,g_num(1,ig))=width((ii-1)+j)
           g_coord(1,g_num(2,ig))=width((ii-1)+j)
           g_coord(1,g_num(3,ig))=width((ii-1)+j)
           g_coord(1,g_num(5,ig))=width((ii-1)+j+1)
           g_coord(1,g_num(6,ig))=width((ii-1)+j+1)
           g_coord(1,g_num(7,ig))=width((ii-1)+j+1)
           g_coord(1,g_num(4,ig))=                                        &
             pt5*(g_coord(1,g_num(3,ig))+g_coord(1,g_num(5,ig)))
           g_coord(1,g_num(8,ig))=                                        &
             pt5*(g_coord(1,g_num(1,ig))+g_coord(1,g_num(7,ig)))
           g_coord(2,g_num(1,ig))=depth((ii-1)+i)
           g_coord(2,g_num(8,ig))=depth((ii-1)+i)
           g_coord(2,g_num(7,ig))=depth((ii-1)+i)
           g_coord(2,g_num(3,ig))=depth((ii-1)+i+1)
           g_coord(2,g_num(4,ig))=depth((ii-1)+i+1)
           g_coord(2,g_num(5,ig))=depth((ii-1)+i+1)
           g_coord(2,g_num(2,ig))=                                        &
             pt5*(g_coord(2,g_num(1,ig))+g_coord(2,g_num(3,ig)))
           g_coord(2,g_num(6,ig))=                                        &
             pt5*(g_coord(2,g_num(5,ig))+g_coord(2,g_num(7,ig)))
         END IF
       END DO
     END DO
   END DO
 END IF
RETURN
END SUBROUTINE fmcoem                       
