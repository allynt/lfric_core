!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Contains the quadrature_3d_xyoz_type that accepts a quadrature rule

!> @details This module contains the quadrature_3d_xyoz_type where the weights
!> and points are stored in 2D (x-y - horizontal) and 1D (z - vertical). To
!> construct this type a quadrature rule (quadrature_rule_type) is passed.
!> Currently there are two supported rules: Gaussian and Newton-Cotes.

module quadrature_3d_xyoz_mod
use constants_mod,           only: r_def, i_def, PI, EPS
use log_mod,                 only: LOG_LEVEL_ERROR, log_event, log_scratch_space
use quadrature_rule_mod,     only: quadrature_rule_type
use quadrature_abstract_mod, only: quadrature_abstract_type

implicit none
private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
type, public, extends(quadrature_abstract_type) :: quadrature_3d_xyoz_type
  private
  !> allocatable arrays which holds the quadrature weights (w) and points (x)
  real(kind=r_def), allocatable :: xqp_xy(:,:), xqp_z(:), wqp_xy(:), wqp_z(:)

  !> Number of quadrature points xy direction = nqp_x*nqp_y
  integer(kind=i_def) :: nqp_xy

  !> Number of quadrature points in each direction
  integer(kind=i_def) :: nqp_x, nqp_y, nqp_z

  !> True if the number of quadrature points is the same in all directions
  logical :: nqp_is_equal = .false.

contains

  !> Function to get a proxy with public pointers to the data in a
  !! quadrature_3d_xyoz type.
  procedure, public :: get_proxy

  !> @brief Routine to destroy quadrature
  final     :: quadrature_destructor

end type quadrature_3d_xyoz_type

!> Psy layer representation of a quadrature_3d_xyoz type
!>
!> This is an accessor class that allows access to quadrature_3d_xyoz_type 
!> data and information with each element accessed via a public pointer.
!>
type, public :: quadrature_3d_xyoz_proxy_type

  private
  !> allocatable arrays which holds the values of the gaussian quadrature
  real(kind=r_def), pointer, public :: xqp_xy(:,:) => null()
  real(kind=r_def), pointer, public :: xqp_z(:)    => null()
  real(kind=r_def), pointer, public :: wqp_xy(:)   => null()
  real(kind=r_def), pointer, public :: wqp_z(:)    => null()

  !> Number of quadrature points xy direction = nqp_x*nqp_y
  integer, pointer, public          :: nqp_xy      => null()

  !> Number of quadrature points in x direction
  integer, pointer, public          :: nqp_x       => null()

  !> Number of quadrature points in y direction
  integer, pointer, public          :: nqp_y       => null()

  !> Number of quadrature points in z direction
  integer, pointer, public          :: nqp_z       => null()

contains
end type quadrature_3d_xyoz_proxy_type

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------

interface quadrature_3d_xyoz_type
  module procedure init_quadrature_uniform
  module procedure init_quadrature_varible
end interface
!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!> @brief Function to create a proxy with access to the data in the
!> quadrature_3d_xyoz_type.
!>
!> @return The proxy type with public pointers to the elements of
!> quadrature_3d_xyoz_type.
type(quadrature_3d_xyoz_proxy_type ) function get_proxy(self)

  implicit none

  class(quadrature_3d_xyoz_type), target, intent(in)  :: self

  get_proxy % xqp_xy              => self % xqp_xy
  get_proxy % xqp_z               => self % xqp_z
  get_proxy % wqp_xy              => self % wqp_xy
  get_proxy % wqp_z               => self % wqp_z
  get_proxy % nqp_xy              => self % nqp_xy
  get_proxy % nqp_x               => self % nqp_x
  get_proxy % nqp_y               => self % nqp_y
  get_proxy % nqp_z               => self % nqp_z

end function get_proxy

!-------------------------------------------------------------------------------
!> @brief Initialises the quadrature rule with equal size in all dimensions.
!> @param[in] nqp integer, The number of points per dimension
!> @param[in] rule quadrature_rule_type, quadrature rule
!-------------------------------------------------------------------------------
function init_quadrature_uniform(nqp, rule) result (self)

  implicit none

  type(quadrature_3d_xyoz_type) :: self
  integer, intent(in) :: nqp
  class(quadrature_rule_type), intent(in) :: rule

  self%nqp_x = nqp
  self%nqp_y = nqp
  self%nqp_z = nqp
  
  self%nqp_xy = nqp**2

  self%nqp_is_equal = .true.

  call create_quadrature( self, rule )

end function init_quadrature_uniform

!-------------------------------------------------------------------------------
!> @breif Initialises the quadrature rule with explicit size in each dimension.
!> @param[in] nqp_x integer, The number of points in x direction
!> @param[in] nqp_y integer, The number of points in y direction
!> @param[in] nqp_z integer, The number of points in z direction
!> @param[in] rule quadrature_rule_type, quadrature rule
!-------------------------------------------------------------------------------
function init_quadrature_varible(nqp_x, nqp_y, nqp_z, rule) result (self)

  implicit none

  type(quadrature_3d_xyoz_type) :: self
  integer, intent(in) :: nqp_x
  integer, intent(in) :: nqp_y
  integer, intent(in) :: nqp_z
  class(quadrature_rule_type), intent(in) :: rule

  self%nqp_x = nqp_x
  self%nqp_y = nqp_y
  self%nqp_z = nqp_z

  self%nqp_xy = nqp_x*nqp_y

  self%nqp_is_equal = (nqp_x == nqp_y .and. nqp_x == nqp_z)
 
  call create_quadrature( self, rule )

end function init_quadrature_varible

!-----------------------------------------------------------------------------
!> @brief Distribute quadrature points (xqp) and weights (wqp)
!> @param[in] self The calling quadrature rule
!> @param[in] rule quadrature_rule_type quadrature rule to use
!> @todo this code is correct for quads but will need modification for
!>       hexes/triangles)
!-----------------------------------------------------------------------------
subroutine create_quadrature(self, rule)

  implicit none

  class(quadrature_3d_xyoz_type) :: self
  class(quadrature_rule_type), intent(in) :: rule

  integer(kind=i_def)    :: i,j,ic
  real(kind=r_def), allocatable       :: points_weights_x(:,:)
  real(kind=r_def), allocatable       :: points_weights_y(:,:)
  real(kind=r_def), allocatable       :: points_weights_z(:,:)

  ! Allocate space for the points of points weights in the quad type
  allocate( self%xqp_z(self%nqp_z) )
  allocate( self%wqp_z(self%nqp_z) )
  allocate( self%xqp_xy(2,self%nqp_xy) )
  allocate( self%wqp_xy(self%nqp_xy) )

  ! Initilise all to zero
  self%xqp_z(:) = 0.0_r_def
  self%wqp_z(:) = 0.0_r_def
  self%xqp_xy(:,:) = 0.0_r_def
  self%wqp_xy(:) = 0.0_r_def

  ! Allocate space for the points and weights of the 1D with dimension defined
  ! in quad type
  allocate( points_weights_x( self%nqp_x,2 ) )
  allocate( points_weights_y( self%nqp_y,2 ) )
  allocate( points_weights_z( self%nqp_z,2 ) )

  ! Get a copy of the 1D points and weights
  points_weights_x = rule % quadrature_rule( self%nqp_x )
  if (self%nqp_is_equal) then
    points_weights_y = points_weights_x
    points_weights_z = points_weights_x
  else
    points_weights_y = rule % quadrature_rule( self%nqp_y )
    points_weights_z = rule % quadrature_rule( self%nqp_z )
  endif

  ! Distribute the 1D points and weights
  ! This is correct for quads (will need modification for hexes/triangles)
  self%xqp_z = points_weights_z(:,1)
  self%wqp_z = points_weights_z(:,2)

  ic = 1
  do i=1,self%nqp_x
    do j=1,self%nqp_y
      self%xqp_xy(1,ic) = points_weights_x(i,1)
      self%xqp_xy(2,ic) = points_weights_y(j,1)
      self%wqp_xy(ic) = points_weights_x(i,2)*points_weights_y(j,2)

      ic = ic + 1
    end do
  end do

  deallocate( points_weights_x, points_weights_y, points_weights_z )

  return
end subroutine create_quadrature

subroutine quadrature_destructor(self)
  implicit none
  type(quadrature_3d_xyoz_type) :: self

  if(allocated(self%xqp_z))  deallocate(self%xqp_z)
  if(allocated(self%xqp_xy)) deallocate(self%xqp_xy)
  if(allocated(self%wqp_z))  deallocate(self%wqp_z)
  if(allocated(self%wqp_xy)) deallocate(self%wqp_xy)
  
end subroutine quadrature_destructor

end module quadrature_3d_xyoz_mod
