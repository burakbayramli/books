INTEGER FUNCTION lnblnk( str )
 CHARACTER*(*),INTENT(IN)::str
 CHARACTER*1 space, tab, null, char
 INTEGER j,i
 DATA space/' '/, tab/'	'/
 null = CHAR(0)
 i = LEN( str )
 DO j = i, 1, -1
  IF(str(j:j).ne.space.and.str(j:j).ne.null.and.str(j:j).ne.tab)THEN
   lnblnk = j
   RETURN
  ENDIF
 ENDDO
 lnblnk = 0
 RETURN
END FUNCTION lnblnk
