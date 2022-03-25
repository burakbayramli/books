SUBROUTINE exc_nods(noexe,exele,g_num,totex,ntote,nf)
!
! This subroutine forms the nodes removed in an excavation lift.
!
 IMPLICIT NONE
 INTEGER,INTENT(IN)::noexe,exele(:),g_num(:,:)
 INTEGER,INTENT(IN OUT)::totex(:),ntote,nf(:,:)
 INTEGER::i,jj,k,iel,modex,nodex,ncheck,nels,nod
 nels=UBOUND(g_num,2)
 nod=UBOUND(g_num,1)
 ntote=ntote+noexe
 totex(ntote-noexe+1:ntote)=exele
 DO i=1,noexe
   DO k=1,8
     nodex=0
     ncheck=g_num(k,exele(i))
     DO iel=1,nels
       modex=0
       DO jj=1,ntote
         IF(iel==totex(jj))THEN
           modex=1
           EXIT
         END IF
       END DO
       IF(modex==1)CYCLE
       DO jj=1,nod
         IF(ncheck==g_num(jj,iel))THEN
           nodex=1
           EXIT
         END IF
       END DO
       IF(nodex==1)EXIT
     END DO
     IF(nodex==0)nf(:,ncheck)=0
   END DO
 END DO
RETURN
END SUBROUTINE exc_nods
