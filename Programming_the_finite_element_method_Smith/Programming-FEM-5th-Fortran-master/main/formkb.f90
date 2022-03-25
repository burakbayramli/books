subroutine formkb(kb,km,g)
! lower triangular global stiffness kb stored as kb(n,iw+1)
 implicit none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in)::km(:,:)
 REAL(iwp),intent(out)::kb(:,:)
 integer,intent(in)::g(:)
 integer::iw,idof,i,j,icd
 idof=size(km,1)
 iw=size(kb,2)-1
 do i=1,idof
   if(g(i)>0)then 
     do j=1,idof
       if(g(j)>0)then
         icd=g(j)-g(i)+iw+1
         if(icd-iw-1<=0)kb(g(i),icd)=kb(g(i),icd)+km(i,j)
       end if
     end do
   end if
 end do
return
end subroutine formkb
