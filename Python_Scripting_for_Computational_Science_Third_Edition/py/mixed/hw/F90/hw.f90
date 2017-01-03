program hwtest
real*8 r1, r2 ,s
r1 = 1.0
r2 = 0.0
s = hw1(r1, r2)
write(*,*) 'hw1, result:',s
write(*,*) 'hw2, result:'
call hw2(r1, r2)
call hw3(r1, r2, s)
write(*,*) 'hw3, result:', s
end

real*8 function hw1(r1, r2)
real*8, intent(in) :: r1, r2
hw1 = sin(r1 + r2)
return
end

subroutine hw2(r1, r2)
real*8, intent(in) :: r1, r2
real*8 :: s  ! result
s = sin(r1 + r2)
write(*,*) 'Hello, World! sin(', r1+r2, ')=', s
return
end

!     special version of hw1 where the result is
!     returned as an argument:

subroutine hw3(r1, r2, s)
real*8, intent(in) :: r1, r2
real*8, intent(out) :: s
s = sin(r1 + r2)
return
end


