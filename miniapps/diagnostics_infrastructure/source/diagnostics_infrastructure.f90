!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @page Miniapp diagnostics_infrastructure program

!> @brief Program used as proof of concept for diagnostics infrastructure. Simple miniapp for easy cannibalising.

!> @details Calls init, run and finalise routines from a driver module

program diagnostics_infrastructure

    use cli_mod, only : get_initial_filename
    use constants_mod, only : i_def
    use diagnostics_infrastructure_driver_mod, only : initialise, run, finalise
    use time_config_mod, only : timestep_start, &
            timestep_end
    use io_config_mod, only : use_xios_io
    use log_mod, only : log_event, &
            log_scratch_space, &
            LOG_LEVEL_INFO, &
            LOG_LEVEL_TRACE
    use xios, only : xios_update_calendar

    implicit none

    character(:), allocatable :: filename
    integer(i_def) :: timestep

    call log_event("starting up", LOG_LEVEL_INFO)
    call get_initial_filename(filename)
    call initialise(filename)
    deallocate(filename)
    ! standard timestepping from gungho
    do timestep = timestep_start, timestep_end
        ! Update XIOS calendar if we are using it for diagnostic output or checkpoint
        if (use_xios_io) then
            call log_event("Gungho: Updating XIOS timestep", LOG_LEVEL_INFO)
            call xios_update_calendar(timestep - timestep_start + 1)
        end if

        !debug
        write(log_scratch_space, '("/", A, "\ ")') repeat("*", 76)
        call log_event(log_scratch_space, LOG_LEVEL_TRACE)
        write(log_scratch_space, '(A,I0)') 'Start of timestep ', timestep
        call log_event(log_scratch_space, LOG_LEVEL_INFO)

        !do it!
        call run()

    end do

    call finalise()

end program diagnostics_infrastructure
