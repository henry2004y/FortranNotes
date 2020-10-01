program main

  real*8 :: a,b,c,d
  real*8 :: func

  a = 2
  b = 4
  c = 0
  d = -1
  call f(a,b,c)
  d = func(a,b)
  write(*,*) 'c=',c
  write(*,*) 'd=',d

  
end program main
