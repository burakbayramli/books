SUBROUTINE formkc(bk,km,cm,g,n)
!global stiffness matrix stored as a vector (upper triangle); complex
IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
REAL(iwp),INTENT(in)::km(:,:),cm(:,:);COMPLEX(iwp),INTENT(out)::bk(:)
INTEGER,INTENT(IN)::g(:),n
INTEGER::idof,i,j,icd,ival
idof=SIZE(km,1)
     DO i=1,idof
        IF(g(i)/=0) THEN
           DO j=1,idof
              IF(g(j)/=0) THEN
                 icd=g(j)-g(i)+1
                 IF(icd-1>=0) THEN
                    ival=n*(icd-1)+g(i)      
                    bk(ival)=bk(ival)+CMPLX(km(i,j),cm(i,j))
                 END IF
               END IF
            END DO
         END IF
     END DO
RETURN
END SUBROUTINE FORMKC 
