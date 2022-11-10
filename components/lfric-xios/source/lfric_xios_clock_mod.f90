!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Provides an interface to the XIOS clock.
!>
module lfric_xios_clock_mod

  use calendar_mod,  only : calendar_type
  use clock_mod,     only : clock_type
  use constants_mod, only : i_timestep, r_second, &
                            l_def
  use timer_mod,     only : timer
  use xios,          only : operator(+),         &
                            operator(-),         &
                            xios_date,           &
                            xios_duration,       &
                            xios_get_start_date, &
                            xios_set_start_date, &
                            xios_set_timestep,   &
                            xios_update_calendar

  implicit none

  private

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Wraps the XIOS clock with the standard clock interface.
  !>
  type, public, extends(clock_type) :: lfric_xios_clock_type
    private
    class(calendar_type), allocatable :: calendar
    logical                           :: uses_timer = .false.
    integer(i_timestep)               :: first_step
    integer(i_timestep)               :: current_step
    integer(i_timestep)               :: last_step
    real(r_second)                    :: seconds_per_step
    logical                           :: initialising
    logical                           :: starting
    integer(i_timestep)               :: first_display_step
    integer(i_timestep)               :: last_display_step
  contains
    private
    procedure, public :: get_first_step
    procedure, public :: get_step
    procedure, public :: get_last_step
    procedure, public :: initial_step
    procedure, public :: tick
    procedure, public :: get_seconds_per_step
  end type lfric_xios_clock_type

  interface lfric_xios_clock_type
    procedure lfric_xios_clock_constructor
  end interface lfric_xios_clock_type

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Sets up an XIOS clock object.
  !>
  !> @param [in] calendar          Interprets human-readable times.
  !> @param [in] first             Time of first step.
  !> @param [in] last              Time of last step.
  !> @param [in] seconds_per_step  Length of a time step in seconds.
  !> @param [in] timer_flag        Flag for use of subroutine timers.
  !>
  function lfric_xios_clock_constructor( calendar,         &
                                         first,            &
                                         last,             &
                                         seconds_per_step, &
                                         timer_flag ) result(new_clock)

    implicit none

    class(calendar_type),         intent(in)    :: calendar
    character(*),                 intent(in)    :: first
    character(*),                 intent(in)    :: last
    real(r_second),               intent(in)    :: seconds_per_step
    logical(l_def), optional,     intent(in)    :: timer_flag
    type(lfric_xios_clock_type) :: new_clock

    type(xios_duration) :: xios_since_timestep_zero, &
                           timestep_length_for_xios
    type(xios_date)     :: xios_start_date

    integer(i_timestep) :: step_offset

    if ( present(timer_flag) ) then
      new_clock%uses_timer = timer_flag
    end if

    ! It may not be necessary to keep the calendar hanging around or even
    ! pass it around. Have a think about this.
    !
    allocate( new_clock%calendar, source=calendar )

    new_clock%first_display_step = new_clock%calendar%parse_instance( first )
    new_clock%last_display_step = new_clock%calendar%parse_instance( last )

    new_clock%first_step = 1
    new_clock%last_step = new_clock%last_display_step &
                          - new_clock%first_display_step + 1
    new_clock%current_step = new_clock%first_step
    new_clock%seconds_per_step = seconds_per_step
    new_clock%initialising = .false.
    new_clock%starting = .true.

    ! Set the current date by adding the run length so far to the run start date
    ! obtained from XIOS
    step_offset = new_clock%get_first_step() - 1
    call xios_get_start_date(xios_start_date)
    xios_since_timestep_zero%second &
        = new_clock%seconds_per_step * step_offset
    xios_start_date = xios_start_date + xios_since_timestep_zero

    call xios_set_start_date(xios_start_date)

    ! Set the XIOS time-step from the model clock
    timestep_length_for_xios%second = new_clock%get_seconds_per_step()
    call xios_set_timestep( timestep_length_for_xios )

  end function lfric_xios_clock_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Performs the first clock step during the intialisation phase - updates
  !> XIOS calendar without ticking model clock
  !>
  subroutine initial_step( this )

    use xios, only : xios_get_current_date

    implicit none

    class(lfric_xios_clock_type), intent(inout) :: this
    type(xios_date) :: date

    this%initialising = .true.

    if ( this%uses_timer ) call timer('xios_update_calendar')
    call xios_update_calendar( this%get_step() - this%get_first_step() + 1 )
    call xios_get_current_date( date )
    if ( this%uses_timer ) call timer('xios_update_calendar')

  end subroutine initial_step

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Advances the clock by one step.
  !>
  function tick( this )

    use xios, only : xios_get_current_date

    implicit none

    class(lfric_xios_clock_type), intent(inout) :: this
    logical :: tick
    type(xios_date) :: date

    if (this%starting) then
      this%starting = .false.
      this%initialising = .false.
    else
      if (this%current_step < this%last_step) then
        this%current_step = this%current_step + 1
      end if
    end if

    if ( this%uses_timer ) call timer('xios_update_calendar')
    call xios_update_calendar( this%current_step - this%first_step + 1)
    tick = (this%current_step <= this%last_step)
    call xios_get_current_date( date )
    if ( this%uses_timer ) call timer('xios_update_calendar')

  end function tick

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the first step the clock will make.
  !>
  !> This is not the first step XIOS will take which is always "1".
  !>
  function get_first_step( this )

    implicit none

    class(lfric_xios_clock_type), intent(in) :: this
    integer(i_timestep) :: get_first_step

    get_first_step = this%first_display_step

  end function get_first_step

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the current step number.
  !>
  !> This is not the step which XIOS will take as it always starts with "1".
  !>
  function get_step( this )

    implicit none

    class(lfric_xios_clock_type), intent(in) :: this
    integer(i_timestep) :: get_step

    get_step = this%first_display_step + this%current_step - 1

  end function get_step

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the last step in this clock's run.
  !>
  !> This is not the last step XIOS will take as it always starts at "1".
  !>
  function get_last_step( this )

    implicit none

    class(lfric_xios_clock_type), intent(in) :: this
    integer(i_timestep) :: get_last_step

    get_last_step = this%last_display_step

  end function get_last_step

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the number of seconds in a step.
  !>
  function get_seconds_per_step( this )

    implicit none

    class(lfric_xios_clock_type), intent(in) :: this
    real(r_second) :: get_seconds_per_step

    get_seconds_per_step = this%seconds_per_step

  end function get_seconds_per_step

end module lfric_xios_clock_mod
