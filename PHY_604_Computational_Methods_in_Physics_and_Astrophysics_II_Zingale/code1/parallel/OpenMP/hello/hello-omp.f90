program hello

  !$OMP parallel
  print *, "Hello world"
  !$OMP end parallel

end program hello
