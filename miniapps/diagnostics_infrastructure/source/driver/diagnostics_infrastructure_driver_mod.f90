!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Drives the execution of the diagnostics_infrastructure miniapp.
!>
!> This is a temporary solution until we have a proper driver layer.
!>
module diagnostics_infrastructure_driver_mod

    use constants_mod, only : i_def, i_native
    use convert_to_upper_mod, only : convert_to_upper
    use cli_mod, only : get_initial_filename
    use create_mesh_mod, only : init_mesh
    use create_fem_mod, only : init_fem
    use init_diagnostics_infrastructure_mod, only : init_diagnostics_infrastructure
    use seed_diagnostics_infrastructure_mod, only : seed_diagnostics_infrastructure
    use yaxt, only : xt_initialize, xt_finalize
    use global_mesh_collection_mod, only : global_mesh_collection, &
            global_mesh_collection_type
    use field_mod, only : field_type, field_proxy_type
    use diagnostics_infrastructure_alg_mod, only : diagnostics_infrastructure_alg
    use configuration_mod, only : final_configuration
    use diagnostics_infrastructure_configuration_mod, only : load_configuration, program_name
    use derived_config_mod, only : set_derived_config
    use log_mod, only : log_event, &
            log_set_level, &
            log_scratch_space, &
            initialise_logging, &
            finalise_logging, &
            LOG_LEVEL_ALWAYS, &
            LOG_LEVEL_ERROR, &
            LOG_LEVEL_WARNING, &
            LOG_LEVEL_INFO, &
            LOG_LEVEL_DEBUG, &
            LOG_LEVEL_TRACE
    use io_config_mod, only : write_diag, &
            use_xios_io
    use diagnostics_io_mod, only : write_scalar_diagnostic
    use io_mod, only : xios_domain_init
    use checksum_alg_mod, only : checksum_alg
    use mpi_mod, only : initialise_comm, store_comm, &
            finalise_comm, &
            get_comm_size, get_comm_rank

    use xios, only : xios_context_finalize, &
            xios_finalize, &
            xios_initialize, &
            xios_update_calendar
    use mod_wait, only : init_wait

    implicit none

    private
    public initialise, run, finalise

    ! Prognostic fields
    type(field_type) :: red, green, blue
    ! Diagnostic fields
    type(field_type) :: hex

    ! Coordinate field
    type(field_type), target, dimension(3) :: chi

    integer(i_def) :: mesh_id, twod_mesh_id

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> Sets up required state in preparation for run.
    !> mostly boiler plate - note the init and seeding of the fields at the end of the function
    !>
    subroutine initialise(filename)

        use logging_config_mod, only : run_log_level, &
                key_from_run_log_level, &
                RUN_LOG_LEVEL_ERROR, &
                RUN_LOG_LEVEL_INFO, &
                RUN_LOG_LEVEL_DEBUG, &
                RUN_LOG_LEVEL_TRACE, &
                RUN_LOG_LEVEL_WARNING

        implicit none

        character(:), intent(in), allocatable :: filename

        character(len = *), parameter :: xios_id = "lfric_client"
        character(len = *), parameter :: xios_ctx = "diagnostics_infrastructure"

        integer(i_def) :: total_ranks, local_rank
        integer(i_def) :: comm = -999
        integer(i_def) :: dtime

        integer(i_native) :: log_level

        ! Initialse mpi and create the default communicator: mpi_comm_world
        call initialise_comm(comm)

        ! Initialise XIOS and get back the split mpi communicator
        call init_wait()
        call xios_initialize(xios_id, return_comm = comm)

        !Store the MPI communicator for later use
        call store_comm(comm)

        ! Initialise YAXT
        call xt_initialize(comm)

        !Store the MPI communicator for later use
        call store_comm(comm)

        ! and get the rank information from the virtual machine
        total_ranks = get_comm_size()
        local_rank = get_comm_rank()

        call initialise_logging(local_rank, total_ranks, program_name)

        call load_configuration(filename)

        select case (run_log_level)
        case(RUN_LOG_LEVEL_ERROR)
            log_level = LOG_LEVEL_ERROR
        case(RUN_LOG_LEVEL_WARNING)
            log_level = LOG_LEVEL_WARNING
        case(RUN_LOG_LEVEL_INFO)
            log_level = LOG_LEVEL_INFO
        case(RUN_LOG_LEVEL_DEBUG)
            log_level = LOG_LEVEL_DEBUG
        case(RUN_LOG_LEVEL_TRACE)
            log_level = LOG_LEVEL_TRACE
        end select

        call log_set_level(log_level)

        write(log_scratch_space, '(A)')                            &
                'Runtime message logging severity set to log level: ' // &
                        convert_to_upper(key_from_run_log_level(run_log_level))
        call log_event(log_scratch_space, LOG_LEVEL_ALWAYS)

        call set_derived_config(.true.)


        !-------------------------------------------------------------------------
        ! Model init
        !-------------------------------------------------------------------------
        call log_event('Initialising ' // program_name // ' ...', LOG_LEVEL_ALWAYS)

        allocate(global_mesh_collection, &
                source = global_mesh_collection_type())

        ! Create the mesh
        call init_mesh(local_rank, total_ranks, mesh_id, twod_mesh_id)

        ! Full global meshes no longer required, so reclaim
        ! the memory from global_mesh_collection
        write(log_scratch_space, '(A)') &
                "Purging global mesh collection."
        call log_event(log_scratch_space, LOG_LEVEL_INFO)
        deallocate(global_mesh_collection)

        ! Create FEM specifics (function spaces and chi field)
        call init_fem(mesh_id, chi)

        !-------------------------------------------------------------------------
        ! IO init
        !-------------------------------------------------------------------------

        ! If using XIOS for diagnostic output or checkpointing, then set up
        ! XIOS domain and context

        if (use_xios_io) then

            dtime = 1

            call xios_domain_init(xios_ctx, &
                    comm, &
                    dtime, &
                    mesh_id, &
                    twod_mesh_id, &
                    chi)

            ! Make sure XIOS calendar is set to timestep 1 as it starts there
            ! not timestep 0.
            call xios_update_calendar(1)

        end if


        ! Create and initialise prognostic fields
        call init_diagnostics_infrastructure(mesh_id, twod_mesh_id, chi, red, green, blue, hex)

        call log_event("seed starting values", LOG_LEVEL_INFO)
        ! Seed values as this is a test!
        call seed_diagnostics_infrastructure(red, green, blue)

        call log_event("finish init", LOG_LEVEL_INFO)

    end subroutine initialise

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> Performs time steps.
    !>
    subroutine run()

        implicit none

        call log_event('Running ' // program_name // ' ...', LOG_LEVEL_ALWAYS)

        ! Call an algorithm
        call diagnostics_infrastructure_alg(red, green, blue, hex)


        ! Write out output file
        call log_event("diagnostics_infrastructure: Writing diagnostic output", LOG_LEVEL_INFO)

        if (write_diag) then
            ! Calculation and output of diagnostics
        call log_event("write_red", LOG_LEVEL_INFO)
            call red%write_field('diagnostics_infrastructure_red')
        call log_event("write_green", LOG_LEVEL_INFO)
            call green%write_field('diagnostics_infrastructure_green')
        call log_event("write_blue", LOG_LEVEL_INFO)
            call blue%write_field('diagnostics_infrastructure_blue')
        call log_event("write_hex", LOG_LEVEL_INFO)
            call hex%write_field('diagnostics_infrastructure_hex')
        end if

    end subroutine run

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> Tidies up after a run.
    !>
    subroutine finalise()

        implicit none

        !-----------------------------------------------------------------------------
        ! Model finalise
        !-----------------------------------------------------------------------------
        call log_event('Finalising ' // program_name // ' ...', LOG_LEVEL_ALWAYS)

        ! Write checksums to file
        call checksum_alg('diagnostics_infrastructure', red, 'diagnostics_infrastructure_red')
        call checksum_alg('diagnostics_infrastructure', green, 'diagnostics_infrastructure_green')
        call checksum_alg('diagnostics_infrastructure', blue, 'diagnostics_infrastructure_blue')
        call checksum_alg('diagnostics_infrastructure', hex, 'diagnostics_infrastructure_hex')

        !-------------------------------------------------------------------------
        ! Driver layer finalise
        !-------------------------------------------------------------------------

        ! Finalise XIOS context if we used it for diagnostic output or checkpointing
        if (use_xios_io) then
            call xios_context_finalize()
        end if

        ! Finalise XIOS
        call xios_finalize()

        ! Finalise namelist configurations
        call final_configuration()

        ! Finalise YAXT
        call xt_finalize()

        ! Finalise mpi and release the communicator
        call finalise_comm()

        call log_event(program_name // ' completed.', LOG_LEVEL_ALWAYS)

        ! Finalise the logging system
        call finalise_logging()

    end subroutine finalise

end module diagnostics_infrastructure_driver_mod
