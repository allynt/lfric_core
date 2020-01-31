!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @page Miniapp diagnostics program

!> @brief Program used as proof of concept for testing core infrastructure. Simple miniapp for easy cannibalising.

!> @details Calls init, run and finalise routines from a driver module

program diagnostics

    use cli_mod, only : get_initial_filename
    use diagnostics_driver_mod, only : initialise, run, finalise

    use log_mod, only : log_event, LOG_LEVEL_INFO

    implicit none

    character(:), allocatable :: filename

    call log_event("starting up", LOG_LEVEL_INFO)
    call get_initial_filename(filename)
    call initialise(filename)
    deallocate(filename)

    !do it!
    call run()

    call finalise()

end program diagnostics
