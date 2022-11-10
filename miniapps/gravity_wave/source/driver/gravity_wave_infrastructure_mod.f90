!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Controls infrastructure related information used by the model

module gravity_wave_infrastructure_mod

  use check_configuration_mod,    only : get_required_stencil_depth
  use cli_mod,                    only : get_initial_filename
  use clock_mod,                  only : clock_type
  use configuration_mod,          only : final_configuration
  use constants_mod,              only : i_def, i_native, &
                                         PRECISION_REAL,  &
                                         r_def, r_second
  use convert_to_upper_mod,       only : convert_to_upper
  use derived_config_mod,         only : set_derived_config
  use gravity_wave_mod,           only : load_configuration, program_name
  use lfric_xios_clock_mod,       only : lfric_xios_clock_type
  use log_mod,                    only : log_event,          &
                                         log_scratch_space,  &
                                         LOG_LEVEL_ALWAYS,   &
                                         LOG_LEVEL_INFO
  use mesh_mod,                   only : mesh_type
  use model_clock_mod,            only : model_clock_type
  use mpi_mod,                    only : get_comm_size, get_comm_rank
  use io_context_mod,             only : io_context_type
  use io_context_mod,             only : io_context_type
  use field_mod,                  only : field_type
  use driver_comm_mod,            only : init_comm, final_comm
  use driver_fem_mod,             only : init_fem
  use driver_io_mod,              only : init_io, final_io, get_io_context
  use driver_mesh_mod,            only : init_mesh
  use driver_log_mod,             only : init_logger, final_logger
  use runtime_constants_mod,      only : create_runtime_constants
  use formulation_config_mod,     only : l_multigrid

  implicit none

  private
  public initialise_infrastructure, finalise_infrastructure

  character(*), public, parameter   :: xios_ctx = 'gravity_wave'

contains

  !> @brief Initialises the infrastructure used by the model
  !> @param [in]     program_name  An identifier given to the model begin run
  !> @param [in,out] mesh          The model prime mesh
  !> @param [in,out] twod_mesh     The model prime 2D mesh
  subroutine initialise_infrastructure( program_name, &
                                        mesh,         &
                                        twod_mesh,    &
                                        model_clock )

    use lfric_xios_context_mod,  only : lfric_xios_context_type
    use log_mod,                 only : log_level_error
    use step_calendar_mod,       only : step_calendar_type
    use time_config_mod,         only : timestep_start, timestep_end
    use timestepping_config_mod, only : dt, spinup_period

    implicit none

    character(*),           intent(in) :: program_name
    type(mesh_type),        intent(inout), pointer :: mesh
    type(mesh_type),        intent(inout), pointer :: twod_mesh
    type(model_clock_type), intent(out)            :: model_clock

    class(io_context_type), pointer :: io_context => null()
    class(clock_type),      pointer :: io_clock => null()
    type(step_calendar_type)        :: step_calendar

    type(field_type), target :: chi(3)
    type(field_type), target :: panel_id

    integer(i_def),   allocatable :: multigrid_mesh_ids(:)
    integer(i_def),   allocatable :: multigrid_2d_mesh_ids(:)
    type(field_type), allocatable :: chi_mg(:,:)
    type(field_type), allocatable :: panel_id_mg(:)

    integer(i_native)          :: comm

    character(:), allocatable :: filename

    ! Set up the communicator for later use
    call init_comm(program_name, comm)

    call get_initial_filename( filename )
    call load_configuration( filename )

    call init_logger(get_comm_rank(), get_comm_size(), program_name)

    write(log_scratch_space,'(A)')                        &
        'Application built with '//trim(PRECISION_REAL)// &
        '-bit real numbers'
    call log_event( log_scratch_space, LOG_LEVEL_ALWAYS )

    call set_derived_config( .false. )

    !-------------------------------------------------------------------------
    ! Initialise aspects of the grid
    !-------------------------------------------------------------------------
    ! Create the mesh
    call init_mesh( get_comm_rank(), get_comm_size(), mesh,        &
                    twod_mesh             = twod_mesh,             &
                    multigrid_mesh_ids    = multigrid_mesh_ids,    &
                    multigrid_2D_mesh_ids = multigrid_2D_mesh_ids, &
                    use_multigrid         = l_multigrid,           &
                    input_stencil_depth   = get_required_stencil_depth() )

    ! Create FEM specifics (function spaces and chi field)
    call init_fem( mesh, chi, panel_id,                           &
                   multigrid_mesh_ids    = multigrid_mesh_ids,    &
                   multigrid_2D_mesh_ids = multigrid_2D_mesh_ids, &
                   chi_mg                = chi_mg,                &
                   panel_id_mg           = panel_id_mg,           &
                   use_multigrid         = l_multigrid )

    !-------------------------------------------------------------------------
    ! Initialise aspects of output
    !-------------------------------------------------------------------------
    call init_io( xios_ctx, comm, chi, panel_id )

    ! Initialise clock
    !
    io_context => get_io_context()
    select type(io_context)
    class is (lfric_xios_context_type)
      io_clock => io_context%get_clock()
      model_clock = model_clock_type( io_clock%get_first_step(),       &
                                      io_clock%get_last_step(),        &
                                      io_clock%get_Seconds_per_step(), &
                                      0.0_r_second )
      call model_clock%add_clock( io_clock )
    class default
      model_clock = model_clock_type(                     &
        step_calendar%parse_instance( timestep_start ),   &
        step_calendar%parse_instance( timestep_end ),     &
        real(dt, r_second), real(spinup_period, r_second) &
      )
    end select

    !-------------------------------------------------------------------------
    ! Setup constants
    !-------------------------------------------------------------------------

    ! Create runtime_constants object. This in turn creates various things
    ! needed by the timestepping algorithms such as mass matrix operators, mass
    ! matrix diagonal fields and the geopotential field and limited area masks.
    call create_runtime_constants( mesh, twod_mesh,                        &
                                   chi, panel_id, model_clock,             &
                                   mg_mesh_ids    = multigrid_mesh_ids,    &
                                   mg_2D_mesh_ids = multigrid_2D_mesh_ids, &
                                   chi_mg         = chi_mg,                &
                                   panel_id_mg    = panel_id_mg )


  end subroutine initialise_infrastructure


  !> @brief Finalises infrastructure used by the model
  subroutine finalise_infrastructure()

    implicit none

    ! Finalise I/O
    call final_io()

    ! Finalise namelist configurations
    call final_configuration()

    ! Finalise the logging system
    call final_logger( program_name )

    ! Finalise communicator
    call final_comm()

  end subroutine finalise_infrastructure

end module gravity_wave_infrastructure_mod
