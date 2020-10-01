PROGRAM fortran_side
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_F_POINTER, C_INT
  IMPLICIT NONE
  INTERFACE
    FUNCTION create_storage() BIND(C, NAME='create_storage')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      IMPLICIT NONE
      TYPE(C_PTR) :: create_storage
    END FUNCTION create_storage
    SUBROUTINE destroy_storage(p) BIND(C, NAME='destroy_storage')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN), VALUE :: p
    END SUBROUTINE destroy_storage
  END INTERFACE
  TYPE(C_PTR) :: p
  INTEGER(C_INT), POINTER :: array(:)
  !****
  p = create_storage()
  CALL C_F_POINTER(p, array, [4])   ! 4 is the array size.
  ! Work with array...
  CALL destroy_storage(p)
END PROGRAM fortran_side
