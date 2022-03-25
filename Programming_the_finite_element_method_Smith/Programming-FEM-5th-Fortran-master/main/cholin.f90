subroutine cholin(kb)
! Choleski reduction on kb(l,iw+1) stored as a lower triangle
 implicit none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in out)::kb(:,:)
 integer::i,j,k,l,ia,ib,n,iw
 REAL(iwp)::x
 n=ubound(kb,1)
 iw=ubound(kb,2)-1
 do i=1,n
   x=.0
   do j=1,iw
     x=x+kb(i,j)**2
   end do
   kb(i,iw+1)=sqrt(kb(i,iw+1)-x)
   do k=1,iw
     x=.0
     if(i+k<=n)then
       if(k/=iw)then
         do l=iw-k,1,-1
           x=x+kb(i+k,l)*kb(i,l+k)
         end do
       end if
       ia=i+k
       ib=iw-k+1
       kb(ia,ib)=(kb(ia,ib)-x)/kb(i,iw+1)
     end if
   end do
 end do
return
end subroutine cholin  
