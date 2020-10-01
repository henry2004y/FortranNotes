program main

implicit none

real, allocatable, target :: State_VGB(:,:,:,:,:)
real, allocatable, save:: Primitive_VG(:,:,:,:)

integer, parameter :: MaxBlock = 512

! Number of variables without energy:                                          
integer, parameter :: nVar = 9

integer, parameter :: nI = 8, nJ = 8, nK = 8
integer, parameter :: nG = 2

! Maximum dimensionality of grid is 3 (cannot be modified)                     
integer, parameter :: MaxDim = 3

! 0 or 1 if a given dimension is ignored or used                               
integer, parameter:: iDim_ = min(nI - 1, 1)
integer, parameter:: jDim_ = min(nJ - 1, 1)
integer, parameter:: kDim_ = min(nK - 1, 1)

  ! Number of not ignored dimensions                                             
integer, parameter:: nDim = iDim_ + jDim_ + kDim_

! Index names limited by nDim                                                  
integer, parameter:: Dim1_=1, Dim2_=min(nDim,2), Dim3_=min(nDim,3)

! Number of nodes per block in each direction                                  
integer, parameter:: nINode = nI + iDim_
integer, parameter:: nJNode = nJ + jDim_
integer, parameter:: nKNode = nK + kDim_

! Number of ghost cells in each direction                                      
integer, parameter:: nGI = nG*iDim_
integer, parameter:: nGJ = nG*jDim_
integer, parameter:: nGK = nG*kDim_

! Cell index ranges including ghost cells                                      
integer, parameter :: &
     MinI = 1 - nGI, MaxI = nI + nGI, &
     MinJ = 1 - nGJ, MaxJ = nJ + nGJ, &
     MinK = 1 - nGK, MaxK = nK + nGK

integer :: iBlock

allocate(State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
allocate(Primitive_VG(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK))

State_VGB = 0.0

! array alignment problem?
do iBlock=1,MaxBlock

    Primitive_VG = State_VGB(:,:,:,:,iBlock)
end do

end program main
