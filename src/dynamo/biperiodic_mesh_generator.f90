!-----------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-----------------------------------------------------------------------------
! Generate a biperiodic mesh and write it to a UGRID format file.
!
! Usage:
!     cubedsphere_mesh_generator <filename>
!
!     filename - Controlling namelist file
!
program biperiodic_mesh_generator

  use biperiodic_mesh_generator_config_mod,                              &
                         only : read_biperiodic_mesh_generator_namelist, &
                                cells_in_x, cells_in_y,                  &
                                cell_width, cell_height,                 &
                                mesh_filename
  use cli_mod,           only : get_initial_filename
  use constants_mod,     only : i_def, r_def, str_def
  use genbiperiodic_mod, only : genbiperiodic_type
  use io_utility_mod,    only : open_file, close_file
  use iso_fortran_env,   only : output_unit
  use ncdf_quad_mod,     only : ncdf_quad_type
  use ugrid_2d_mod,      only : ugrid_2d_type
  use ugrid_file_mod,    only : ugrid_file_type

  implicit none

  character(:), allocatable :: filename
  integer                   :: namelist_unit

  type(genbiperiodic_type)               :: bpgen
  type(ugrid_2d_type)                    :: ugrid_2d
  class(ugrid_file_type), allocatable    :: ugrid_file
  character(len=str_def)                 :: sztext
  integer                                :: fsize

  call get_initial_filename( filename )
  namelist_unit = open_file( filename )
  call read_biperiodic_mesh_generator_namelist( namelist_unit )
  call close_file( namelist_unit )
  deallocate( filename )

  allocate(ncdf_quad_type::ugrid_file)
  call ugrid_2d%set_file_handler(ugrid_file)

  bpgen = genbiperiodic_type(cells_in_x, cells_in_y, cell_width, cell_height)

  write(output_unit, "(A)") "Generating biperiodic mesh with..."
  write(output_unit, "(A,I5)") "  cells_in_x: ", cells_in_x
  write(output_unit, "(A,I5)") "  cells_in_y: ", cells_in_y
  write(output_unit, "(A,F6.1)") "  cell_width: ", cell_width
  write(output_unit, "(A,F6.1)") "  cell_height: ", cell_height

  call ugrid_2d%set_by_generator(bpgen)
  write(output_unit, "(A)") "...generation complete."

  write(output_unit,                             &
        '("Writing ugrid mesh to ", A, " ...")', &
        advance="NO") trim(adjustl(mesh_filename))
  call ugrid_2d%write_to_file(trim(mesh_filename))
  inquire(file=mesh_filename, size=fsize)
  write(sztext, *) fsize
  write(output_unit, '("... ", A, " bytes written.")') trim(adjustl(sztext))

end program biperiodic_mesh_generator
