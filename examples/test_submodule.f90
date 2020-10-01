MODULE m                          ! The ancestor module m
  INTEGER :: i

  INTERFACE
    MODULE SUBROUTINE sub1(arg1)  ! Module procedure interface body for sub1
      INTEGER :: arg1
    END SUBROUTINE
    
    MODULE SUBROUTINE sub2(arg2)  ! Module procedure interface body for sub2
      INTEGER :: arg2
    END SUBROUTINE
  END INTERFACE
END MODULE

SUBMODULE (m) n                   ! The descendant submodule n
  INTEGER :: j                    ! Specification part

  CONTAINS                        ! Module subprogram part
    MODULE SUBROUTINE sub1(arg1)  ! Definition of sub1 by subroutine subprogram
      INTEGER :: arg1
      arg1 = 1
      i = 2                       ! Host association
      j = 3                       ! Host association
    END SUBROUTINE

    MODULE PROCEDURE sub2         ! Definition of sub2 by separate module subprogram
      arg2 = 1
    END PROCEDURE
END SUBMODULE
