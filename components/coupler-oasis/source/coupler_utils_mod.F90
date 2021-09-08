!-------------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief additional routines needed by coupling code
module coupler_utils_mod

  use constants_mod,                  only : i_def, l_def
  implicit none

  private
  public bubble_sort

  contains

!>@brief Sorts index for domain definition for OASIS
!> @param[in,out] array the array that will be sorted
!> @param[in,out] indx index to sort the array
!> @param[in]     lens number of elements in array
  subroutine bubble_sort(array, indx, lens)
!taken from mesh/partition_mod.F90 and modified
    implicit none

    integer(i_def), intent(inout) :: array(:)
    integer(i_def), intent(inout) :: indx(:)
    integer(i_def), intent(in)    :: lens

    logical(l_def) :: swapped   ! temporary variable for data swapping
    integer(i_def) :: i         ! loop index
    integer(i_def) :: swap_temp ! emporary variables keeps swaped data
    integer(i_def) :: swap_indx ! temporary variables keeps swaped index

    do i = 1,lens
      indx(i) = i
    enddo

    do
      swapped = .false.
      do i = 1,lens-1
        if(array(i) > array(i+1))then
          swap_temp = array(i)
          array(i) = array(i+1)
          array(i+1) = swap_temp
          swap_indx = indx(i)
          indx(i) = indx(i+1)
          indx(i+1) = swap_indx
          swapped = .true.
        end if
      end do
      if( .not.swapped )exit
    end do

  end subroutine bubble_sort
end module coupler_utils_mod

