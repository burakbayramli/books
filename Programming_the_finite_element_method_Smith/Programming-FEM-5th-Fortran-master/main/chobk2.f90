 subroutine chobk2(kb,loads)
!Choleski back-substitution
 implicit none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in)::kb(:,:)
 REAL(iwp),intent(in out)::loads(0:)
 integer::iw,n,i,j,l,m
 REAL(iwp)::x
 n=size(kb,1)
 iw=size(kb,2)-1
 loads(n)=loads(n)/kb(n,iw+1)
 do i=n-1,1,-1
   x=0.0
   l=i+iw
   if(i>n-iw)l=n
   m=i+1
   do j=m,l
     x=x+kb(j,iw+i-j+1)*loads(j)
   end do
   loads(i)=(loads(i)-x)/kb(i,iw+1)
 end do
 return
 end subroutine chobk2