!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at
! https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Abstract base quadrature type.
!-------------------------------------------------------------------------------
!> @brief Abstract base type for for quadrature
!> It should be quadrature_mod but that is taken by the original
!> quadrature_type
module quadrature_abstract_mod
use constants_mod,           only: i_def
implicit none
private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, abstract :: quadrature_abstract_type
  private

end type

!-------------------------------------------------------------------------------
! Interfaces
!-------------------------------------------------------------------------------

end module quadrature_abstract_mod

