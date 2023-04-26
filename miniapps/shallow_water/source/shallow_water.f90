!-----------------------------------------------------------------------------
! (c) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @page shallow_water Shallow water equations miniapp
!> This is code that uses the LFRic infrastructure to build a shallow water
!> model that includes some of the GungHo routines.
!>
!> @brief Main program used to simulate shallow water equations.
!>
!> @details This top-level code simply calls initialise, run and finalise
!>          routines that are required to run the model.

program shallow_water

  use cli_mod,                  only: get_initial_filename
  use shallow_water_driver_mod, only: initialise, &
                                      run,        &
                                      finalise

  implicit none

  character(:), allocatable :: filename

  call get_initial_filename( filename )

  call initialise( filename )

  call run()

  call finalise()

end program shallow_water
