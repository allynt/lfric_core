!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief init functionality for the io_dev miniapp

!> @details Handles creation and initialisation of IO test fields
!>
module io_dev_init_mod

  ! Infrastructure
  use constants_mod,                  only : i_def, str_def
  use field_mod,                      only : field_type, field_proxy_type
  use field_parent_mod,               only : field_parent_type, read_interface, write_interface
  use field_collection_mod,           only : field_collection_type, field_collection_iterator_type
  use function_space_collection_mod,  only : function_space_collection
  use fs_continuity_mod,              only : W0, W2H, W2V, Wtheta, W3
  use log_mod,                        only : log_event, &
                                             LOG_LEVEL_INFO
  use pure_abstract_field_mod,        only : pure_abstract_field_type
  ! Configuration
  use finite_element_config_mod,      only : element_order
  use initialization_config_mod,      only : init_option, init_option_fd_start_dump
  ! I/O methods
  use read_methods_mod,               only : read_field_face, &
                                             read_field_single_face, &
                                             read_state
  use write_methods_mod,              only : write_field_node, &
                                             write_field_edge, &
                                             write_field_face, &
                                             write_field_single_face

  implicit none

  private
  public :: create_io_dev_fields, &
            io_dev_init_fields

  contains

  !> @details Creates fields used for inputting and outputting IO_Dev data
  !> @param[in]  mesh_id      The identifier given to the current 3d mesh
  !> @param[in]  twod_mesh_id The identifier given to the current 2d mesh
  !> @param[out] core_fields  The core field collection
  !> @param[out] dump_fields  Collection of fields to be written-to/read-from
  !>                          dump files
  subroutine create_io_dev_fields( mesh_id,      &
                                   twod_mesh_id, &
                                   core_fields,  &
                                   dump_fields )

    implicit none

    ! Arguments
    integer(i_def),              intent(in)  :: mesh_id
    integer(i_def),              intent(in)  :: twod_mesh_id
    type(field_collection_type), intent(out) :: core_fields
    type(field_collection_type), intent(out) :: dump_fields

    ! Local variables
    type(field_type)       :: W0_field
    type(field_type)       :: W2H_field
    type(field_type)       :: W2V_field
    type(field_type)       :: W3_field
    type(field_type)       :: W3_2D_field
    type(field_type)       :: multi_data_field

    ! Pointers
    class(pure_abstract_field_type), pointer :: tmp_field_ptr => null()
    procedure(read_interface),       pointer :: tmp_read_ptr => null()
    procedure(write_interface),      pointer :: tmp_write_ptr => null()

    call log_event( 'IO_Dev: creating model data', LOG_LEVEL_INFO )

    !----------------------------------------------------------------------------
    ! Create core fields to send/recieve data from file and set I/O behaviours
    !----------------------------------------------------------------------------
    ! Create the core and dump field collections.
    core_fields = field_collection_type( name='core_fields' )
    dump_fields = field_collection_type( name='dump_fields' )

    ! W0 (node) field
    call W0_field%initialise( vector_space = &
                   function_space_collection%get_fs(mesh_id, element_order, W0), &
                   name = 'W0_field' )
    tmp_write_ptr => write_field_node
    call W0_field%set_write_behaviour( tmp_write_ptr )
    call core_fields%add_field( W0_field )

    ! W2H (edge) fields
    call W2H_field%initialise( vector_space = &
                   function_space_collection%get_fs(mesh_id, element_order, W2H), &
                   name = 'W2H_field' )
    tmp_write_ptr => write_field_edge
    call W2H_field%set_write_behaviour( tmp_write_ptr )
    call core_fields%add_field( W2H_field )

    call W2V_field%initialise( vector_space = &
                   function_space_collection%get_fs(mesh_id, element_order, W2V), &
                   name = 'W2V_field' )
    tmp_write_ptr => write_field_face
    call W2V_field%set_write_behaviour( tmp_write_ptr )
    call core_fields%add_field( W2V_field )

    ! W3 (face) field
    call W3_field%initialise( vector_space = &
                   function_space_collection%get_fs(mesh_id, element_order, W3), &
                   name = 'W3_field' )
    tmp_write_ptr => write_field_face
    tmp_read_ptr  => read_field_face
    call W3_field%set_write_behaviour( tmp_write_ptr )
    call W3_field%set_read_behaviour( tmp_read_ptr )
    call core_fields%add_field( W3_field )

    ! W3_2D (single_face) field
    call W3_2D_field%initialise( vector_space = &
                   function_space_collection%get_fs(twod_mesh_id, element_order, W3), &
                   name = 'W3_2D_field' )
    tmp_write_ptr => write_field_single_face
    tmp_read_ptr  => read_field_single_face
    call W3_2D_field%set_write_behaviour( tmp_write_ptr )
    call W3_2D_field%set_read_behaviour( tmp_read_ptr )
    call core_fields%add_field( W3_2D_field )

    call multi_data_field%initialise( vector_space = &
                   function_space_collection%get_fs(twod_mesh_id, element_order, W3, ndata=5), &
                   name = 'multi_data_field' )
    tmp_write_ptr => write_field_single_face
    call multi_data_field%set_write_behaviour( tmp_write_ptr )
    call core_fields%add_field( multi_data_field )

    !----------------------------------------------------------------------------
    ! Dump fields
    !----------------------------------------------------------------------------
    ! Add fields to dump_fields collection - fields for which read and write
    ! routines will be tested

    tmp_field_ptr => core_fields%get_field( 'W3_field' )
    call dump_fields%add_reference_to_field( tmp_field_ptr )

    tmp_field_ptr => core_fields%get_field( 'W3_2D_field' )
    call dump_fields%add_reference_to_field( tmp_field_ptr )

    tmp_field_ptr => core_fields%get_field( 'multi_data_field' )
    call dump_fields%add_reference_to_field( tmp_field_ptr )

    nullify( tmp_read_ptr )
    nullify( tmp_write_ptr )
    nullify( tmp_field_ptr )

    call log_event( 'IO_Dev: fields created', LOG_LEVEL_INFO )

  end subroutine create_io_dev_fields

  !> @details Initialises model fields by making data values equal to dof values
  !> @param[in,out] core_fields The core field collection
  subroutine io_dev_init_fields( core_fields )

    implicit none

    ! Arguments
    type(field_collection_type), intent(inout) :: core_fields

    ! Local variables
    type(field_collection_iterator_type) :: iter
    type(field_proxy_type) :: core_proxy
    integer(i_def) :: dof_index

    ! Pointers
    class(field_parent_type), pointer :: fld => null()

    ! Cycle through dump_fields collection
    iter = core_fields%get_iterator()
    do
      if ( .not.iter%has_next() ) exit
      fld => iter%next()

      select type(fld)
        type is (field_type)
        ! Initialise field to default value (dof ID)
        core_proxy = fld%get_proxy()
        do dof_index = 1, core_proxy%vspace%get_last_dof_owned()
          core_proxy%data( dof_index ) = dof_index
        end do
      end select

    end do

    nullify(fld)

  end subroutine io_dev_init_fields

end module io_dev_init_mod
