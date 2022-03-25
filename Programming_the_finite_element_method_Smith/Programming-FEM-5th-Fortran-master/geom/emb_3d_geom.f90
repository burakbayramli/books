SUBROUTINE emb_3d_geom(iel,nx1,nx2,ny1,ny2,nze,w1,s1,w2,h1,h2,d1,coord,num)
! Used in p612, p613
! This subroutine forms the nodal coordinates and numbering for a 3-d
! slope of 20-node hexahedra. Nodes and elements numbered in xz planes
! going in the y-direction.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::w1,s1,w2,h1,h2, d1
 INTEGER,INTENT(IN)::iel,nx1,nx2,ny1,ny2,nze
 REAL(iwp),INTENT(OUT)::coord(:,:)
 INTEGER,intent(OUT)::num(:)
 INTEGER::nxe,nye,nc,nt,nn_mid,nn_main,ip,iq,is,nd,ns
 REAL(iwp)::facx,facy,facz,facb,facs,frh,zero=0.0_iwp,pt5=0.5_iwp
!
!                       basic variable initialization
!
 nxe=nx1+nx2
 nye=ny1+ny2
!number of elements in a slice
 ns=nx1*nye+nx2*ny2
!number of elements in sloped region
 nt=nx1*ny1
!number of nodes in main sloped region
 nc=(3*nye+2)*nx1+2*ny1
!number of nodes in mid sloped region
 nd=(nye+1)*nx1+ny1
!number of nodes in a middle layer
 nn_mid=(nye+1)*(nx1+1)+(ny2+1)*nx2
!number of nodes in a main layer
 nn_main=(3*nye+2)*nx1+2*nye+1+(3*ny2+2)*nx2
!
 facx=s1/ny1
 facy=h1/ny1
 facz=d1/nze
 facb=(w1+s1)/nx1
 facs=zero
 IF(ny2/=0)facs=h2/ny2
 frh=zero
 IF(nx2/=0)frh=w2/nx2
!
!                       determine element index values (ip, iq, is)
!
 is=(iel-1)/ns+1
 IF(iel-(is-1)*ns<=nt)THEN !if it is in sloped region
   iq=(iel-(is-1)*ns-1)/nx1+1
   ip=iel-(is-1)*ns-(iq-1)*nx1
 ELSE                      !if it is in unsloped region
   iq=(iel-(is-1)*ns-nt-1)/nxe+ny1+1
   ip=iel-(is-1)*ns-nt-(iq-ny1-1)*nxe
 END IF
!
! Calculate node indices (num()) and coordinates (coord(,))
 IF(ip<=nx1)THEN !if iel is in sloped region
   num(1)=(ip-1)*(3*nye+2)+2*iq+1+(is-1)*(nn_mid+nn_main)
   num(2)=num(1)-1
   num(3)=num(1)-2
   num(4)=(ip-1)*(3*nye+2)+2*nye+iq+1+(is-1)*(nn_mid+nn_main)
   num(5)=ip*(3*nye+2)+2*iq-1+(is-1)*(nn_mid+nn_main)
   num(6)=num(5)+1
   num(7)=num(5)+2
   num(8)=num(4)+1
   num(9)=(nn_main+(ip-1)*(nye+1)+iq+1)*is+(is-1)*                        &
     (nn_mid-(ip-1)*(nye+1)-iq-1)
   num(10)=num(9)-1
   num(11)=1+(nn_main+ip*(nye+1)+iq-1)*is+(is-1)*(nn_mid-ip*(nye+1)-iq+1)
   num(12)=num(11)+1
   num(13)=(ip-1)*(3*nye+2)+2*iq+1+is*(nn_mid+nn_main)
   num(14)=num(13)-1
   num(15)=num(13)-2
   num(16)=(ip-1)*(3*nye+2)+2*nye+iq+1+is*(nn_mid+nn_main)
   num(17)=ip*(3*nye+2)+2*iq-1+is*(nn_mid+nn_main)
   num(18)=num(17)+1
   num(19)=num(17)+2
   num(20)=num(16)+1
   IF(iq<=ny1)THEN ! if iel is in trapezoidal region
     coord(1,1)=(ip-1)*(w1+iq*facx)/nx1
     coord(3,1)=(ip-1)*(w1+(iq-1)*facx)/nx1
     coord(5,1)=ip*(w1+(iq-1)*facx)/nx1
     coord(7,1)=ip*(w1+iq*facx)/nx1
     coord(1,2)=-iq*facy
     coord(3,2)=-(iq-1)*facy
     coord(5,2)=-(iq-1)*facy
     coord(7,2)=-iq*facy
   ELSE            ! if iel is in rectangular region
     coord(1,1)=(ip-1)*facb
     coord(3,1)=(ip-1)*facb
     coord(5,1)=ip*facb
     coord(7,1)=ip*facb
     coord(1,2)=-h1-(iq-ny1)*facs
     coord(3,2)=-h1-(iq-ny1-1)*facs
     coord(5,2)=-h1-(iq-ny1-1)*facs
     coord(7,2)=-h1-(iq-ny1)*facs
   END IF
 ELSE              ! if iel is in unsloped region
   num(1)=(nn_mid+nn_main)*(is-1)+nc+(3*ny2+2)*(ip-nx1-1)+(iq-ny1)*2+1
   num(2)=num(1)-1
   num(3)=num(1)-2
   num(4)=(nn_mid+nn_main)*(is-1)+nc+(3*ny2+2)*(ip-nx1-1)+2*ny2+1+iq-ny1
   num(5)=(nn_mid+nn_main)*(is-1)+nc+(3*ny2+2)*(ip-nx1)+(iq-ny1)*2-1
   num(6)=num(5)+1
   num(7)=num(5)+2
   num(8)=num(4)+1
   num(9)=(nn_mid+nn_main)*(is-1)+nn_main+nd+(ny2+1)*(ip-nx1-1)+iq-ny1+1
   num(10)=num(9)-1
   num(11)=(nn_mid+nn_main)*(is-1)+nn_main+nd+(ny2+1)*(ip-nx1)+iq-ny1
   num(12)=num(11)+1
   num(13)=(nn_mid+nn_main)*is+nc+(3*ny2+2)*(ip-nx1-1)+(iq-ny1)*2+1
   num(14)=num(13)-1
   num(15)=num(13)-2
   num(16)=(nn_mid+nn_main)*is+nc+(3*ny2+2)*(ip-nx1-1)+2*ny2+1+iq-ny1
   num(17)=(nn_mid+nn_main)*is+nc+(3*ny2+2)*(ip-nx1)+(iq-ny1)*2-1
   num(18)=num(17)+1
   num(19)=num(17)+2
   num(20)=num(16)+1
!
   coord(1,1)=w1+s1+(ip-nx1-1)*frh
   coord(3,1)=coord(1,1)
   coord(5,1)=w1+s1+(ip-nx1)*frh
   coord(7,1)=coord(5,1)
   coord(1,2)=-h1-(iq-ny1)*facs
   coord(3,2)=-h1-(iq-ny1-1)*facs
   coord(5,2)=-h1-(iq-ny1-1)*facs
   coord(7,2)=-h1-(iq-ny1)*facs
 END IF
 coord(9,1)=coord(1,1)
 coord(10,1)=coord(3,1)
 coord(11,1)=coord(5,1)
 coord(12,1)=coord(7,1)
 coord(13,1)=coord(1,1)
 coord(15,1)=coord(3,1)
 coord(17,1)=coord(5,1)
 coord(19,1)=coord(7,1)
 coord(9,2) =coord(1,2)
 coord(10,2)=coord(3,2)
 coord(11,2)=coord(5,2)
 coord(12,2)=coord(7,2)
 coord(13,2)=coord(1,2)
 coord(15,2)=coord(3,2)
 coord(17,2)=coord(5,2)
 coord(19,2)=coord(7,2)
 coord(1:8:1,3)=-(is-1)*facz
 coord(9:12:1,3)=-(is*facz-facz*pt5)
 coord(13:20:1,3)=-is*facz    
 coord(2:6:2,:)=pt5*(coord(1:5:2,:)+coord(3:7:2,:))
 coord(14:18:2,:)=pt5*(coord(13:17:2,:)+coord(15:19:2,:))
 coord(8,:)=pt5*(coord(7,:)+coord(1,:))
 coord(20,:)=pt5*(coord(19,:)+coord(13,:))
RETURN
END SUBROUTINE emb_3d_geom