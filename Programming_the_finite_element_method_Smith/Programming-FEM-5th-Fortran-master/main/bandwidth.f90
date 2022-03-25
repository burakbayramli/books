FUNCTION bandwidth(g) RESULT(nband)
! Used in p91, p101-p103, p112, p115, p810
! This function finds the element bandwidth from g.
!
 IMPLICIT NONE     
 INTEGER,INTENT(IN)::g(:) 
 INTEGER::nband
 nband=MAXVAL(g,1,g>0)-MINVAL(g,1,g>0)
RETURN
END FUNCTION bandwidth

