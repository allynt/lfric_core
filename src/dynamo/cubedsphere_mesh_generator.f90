!-----------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-----------------------------------------------------------------------------
! Generate a cubed-sphere mesh and write it to a UGRID format file.
!
! Usage:
!     cubedsphere_mesh_generator <filename>
!
!     filename - Controlling namelist file
!
program cubedsphere_mesh_generator

  use cli_mod,         only : get_initial_filename
  use constants_mod,   only : i_def, r_def, str_def
  use cubedsphere_mesh_generator_config_mod,                           &
                       only : read_cubedsphere_mesh_generator_namelist, &
                              edge_cells, mesh_filename
  use gencube_mod,     only : gencube_ps_type
  use io_utility_mod,  only : open_file, close_file
  use iso_fortran_env, only : output_unit
  use ncdf_quad_mod,   only : ncdf_quad_type
  use ugrid_file_mod,  only : ugrid_file_type
  use ugrid_2d_mod,    only : ugrid_2d_type

  implicit none

  character(:), allocatable :: filename
  integer                   :: namelist_unit

  type(gencube_ps_type)                  :: csgen
  type(ugrid_2d_type)                    :: ugrid_2d
  class(ugrid_file_type), allocatable    :: ugrid_file
  character(len=str_def)                 :: sztext
  integer                                :: fsize

  call get_initial_filename( filename )
  namelist_unit = open_file( filename )
  call read_cubedsphere_mesh_generator_namelist( namelist_unit )
  call close_file( namelist_unit )
  deallocate( filename )

  allocate(ncdf_quad_type::ugrid_file)
  call ugrid_2d%set_file_handler(ugrid_file)

  csgen = gencube_ps_type( edge_cells )

  write(output_unit, "(A)") "Generating cubed-sphere mesh with..."
  write(output_unit, "(A,I5)") "  ndivs: ", edge_cells

  call ugrid_2d%set_by_generator(csgen)
  write(output_unit, "(A)") "...generation complete."

  write(output_unit,                             &
        '("Writing ugrid mesh to ", A, " ...")', &
        advance="NO") trim(adjustl(mesh_filename))
  call ugrid_2d%write_to_file(trim(mesh_filename))
  inquire(file=mesh_filename, size=fsize)
  write(sztext, *) fsize
  write(output_unit, '("... ", A, " bytes written.")') trim(adjustl(sztext))

end program cubedsphere_mesh_generator
