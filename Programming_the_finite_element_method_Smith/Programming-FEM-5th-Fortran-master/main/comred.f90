SUBROUTINE comred(bk,n)
! gaussian reduction on a vector stored as an upper triangle : complex
IMPLICIT NONE
INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
COMPLEX(iwp),INTENT(in out)::bk(:);INTEGER,INTENT(IN)::n
INTEGER::i,il1,kbl,j,ij,nkb,m,ni,nj,iw ; COMPLEX(iwp)::sum
 iw = UBOUND(bk,1)/n-1
       DO i=2,n
          il1=i-1;kbl=il1+iw+1
          IF(kbl-n>0)kbl=n
          DO j=i,kbl
             ij=(j-i)*n+i;sum=bk(ij);nkb=j-iw
             IF(nkb<=0)nkb=1
             IF(nkb-il1<=0)THEN
                DO m=nkb,il1
                   ni=(i-m)*n+m ; nj=(j-m)*n+m
                   sum=sum-bk(ni)*bk(nj)/bk(m) 
                END DO
             END IF
             bk(ij)=sum
           END DO
       END DO
RETURN
END SUBROUTINE COMRED
