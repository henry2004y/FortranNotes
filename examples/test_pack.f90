PROGRAM test_pack_2
  INTEGER :: m(4)
  m = (/ 1, 0, 0, 2 /)
  WRITE(*, *) pack(m, m /= 0, [ 0, 1, 0, 3, 4 ])  ! "1 2 3 4"
END PROGRAM test_pack_2
  
