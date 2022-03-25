 subroutine chobk1(kb,loads)
!Choleski back-substitution
 implicit none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in)::kb(:,:)
 REAL(iwp),intent(in out)::loads(0:)
 integer::iw,n,i,j,k
 REAL(iwp)::x
 n=size(kb,1)
 iw=size(kb,2)-1
 loads(1)=loads(1)/kb(1,iw+1)
 do i=2,n
   x=.0
   k=1
   if(i<=iw+1)k=iw-i+2
   do j=k,iw
     x=x+kb(i,j)*loads(i+j-iw-1)
   end do
   loads(i)=(loads(i)-x)/kb(i,iw+1)
 end do
return
end subroutine chobk1


