!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Provides an implementation of the Psy layer

!> @details Contains hand-rolled versions of the Psy layer that can be used for
!> simple testing and development of the scientific code

module psykal_lite_integer_maths_mod

    use constants_mod, only : i_def, r_def, imdi
    use field_mod, only : field_type, field_proxy_type
    use log_mod, only : log_event, LOG_LEVEL_ERROR

    implicit none
    public

contains

    !------------------------------------------------------------------------------
    !> invoke_set_target_data_point_kernel: explicitly set a single dof within a target field

    subroutine invoke_integer_inc_X_plus_Y(field1, field2)

        use mesh_mod, only : mesh_type

        implicit none

        type(field_type), intent(inout) :: field1
        type(field_type), intent(in) :: field2

        type(field_proxy_type) :: field1_proxy, field2_proxy
        integer(kind = i_def) :: i, undf

        field1_proxy = field1%get_proxy()
        field2_proxy = field2%get_proxy()

        !safety check
        undf = field1_proxy%vspace%get_last_dof_annexed()
        if(undf /= field2_proxy%vspace%get_last_dof_annexed()) then
            ! they are not on the same function space
            call log_event("Psy:invoke_integer_inc_X_plus_Y:field1 and field2 live on different w-spaces" &
                    , LOG_LEVEL_ERROR)
            !abort
            stop
        endif

        do i = 1, undf
            field1_proxy%data(i) = int(field1_proxy%data(i)) + int(field2_proxy%data(i))
        end do

    end subroutine invoke_integer_inc_X_plus_Y

end module psykal_lite_integer_maths_mod
