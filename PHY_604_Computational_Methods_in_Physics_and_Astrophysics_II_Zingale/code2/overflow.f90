program overflow

  integer i, iold
  !integer*2 i, iold

  iold = -1
  i = 0
  do while (i > iold)
     iold = i
     i = i+1
  enddo

  print *, i

end program overflow

