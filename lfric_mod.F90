  module lfric
    implicit none
    
    
    type :: functionSpace
       integer :: ndf,ncell
       integer, allocatable :: dofmap(:,:)
       ! accessor functions go here
    end type functionSpace

    type :: field
       integer :: ndf,ncell,nlayers
       real, allocatable :: data(:)
       type(functionSpace), pointer :: vspace
       ! accessor function go here
    end type field

    ! overload the default structure constructor for function space
    interface functionSpace
       module procedure constructor
    end interface

    interface field
       module procedure fieldConstructor
    end interface

    !overload the default structure constructure for field
!    interface field
!       module procedure fieldConstructor
!    end interface
    
    public :: functionSpace
    public :: field
  contains

    type(functionSpace) function constructor(ncell,ndf)
      integer, intent(in) :: ncell, ndf
      constructor%ncell = ncell
      constructor%ndf = ndf
      
      ! allocate some space
      allocate(constructor%dofmap(ncell,ndf))
      ! this would need populating 

      return
    end function constructor


    type(field) function fieldConstructor(vectorSpace,num_layers) result(self)
      !------------------------------------------------------------------------
      ! Constructor
      !------------------------------------------------------------------------

      !Arguments      
      integer, intent(in) :: num_layers
      type(functionSpace),  target,intent(in) :: vectorSpace

      self%vspace => vectorSpace

      self%ncell = self%vspace%ncell
      self%ndf = self%vspace%ndf
      self%nlayers = num_layers
      
      ! this is not correct, I need the unique number of dofs
      ! some of these degrees of freedom are shared. What is the
      ! correct formula
      
      allocate(self%data( (self%ncell) * (self%ndf) * (self%nlayers)))
      ! then I need to decide if I do vector and scalar spaces differently

      return
    end function fieldConstructor

! I need some destructors/finalizers. What is F2K3-speak for that.

!    subroutine final_lfric()
!      deallocate( v3dofmap)
!      deallocate( Rv3)
!    end subroutine final_lfric

    
  end module lfric
