!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
module function_space_build_mod
! this build and answer module only works for 3x3 3 biperiodic plane with
! lowest order quads
  use function_space_mod, only : function_space_type, W0, W1, W2, W3, Wtheta,&
                                 W2V, W2H
  use constants_mod, only : r_def

  implicit none 
    type(function_space_type) :: w0_func_space, w1_func_space,      &
                                 w2_func_space, w3_func_space,      &
                                 wtheta_func_space, w2v_func_space, &
                                 w2h_func_space
    integer, allocatable, dimension(:,:), target :: test_map_w0,     &
                                                    test_map_w1,     &
                                                    test_map_w2,     &
                                                    test_map_w3,     &
                                                    test_map_wtheta, &
                                                    test_map_w2v,    &
                                                    test_map_w2h

contains

  subroutine fs_build(mesh)

    use basis_function_mod,         only : &
           w0_nodal_coords, w1_nodal_coords, w2_nodal_coords, w3_nodal_coords, &
           wtheta_nodal_coords, w2v_nodal_coords, w2h_nodal_coords,            &
           w0_basis_order, w0_basis_index, w0_basis_vector, w0_basis_x,        &
           w1_basis_order, w1_basis_index, w1_basis_vector, w1_basis_x,        &
           w2_basis_order, w2_basis_index, w2_basis_vector, w2_basis_x,        &
           w3_basis_order, w3_basis_index, w3_basis_vector, w3_basis_x,        &
           wtheta_basis_order, wtheta_basis_index, wtheta_basis_vector,        &
           wtheta_basis_x,                                                     &
           w2v_basis_order, w2v_basis_index, w2v_basis_vector, w2v_basis_x,    &
           w2h_basis_order, w2h_basis_index, w2h_basis_vector, w2h_basis_x,    &
           w0_dof_on_vert_boundary, w1_dof_on_vert_boundary,                   &
           w2_dof_on_vert_boundary, w3_dof_on_vert_boundary,                   &
           wtheta_dof_on_vert_boundary,                                        &
           w2v_dof_on_vert_boundary, w2h_dof_on_vert_boundary

  use dofmap_mod,              only : &
              w0_dofmap, w1_dofmap, w2_dofmap, w3_dofmap, wtheta_dofmap, &
              w2v_dofmap, w2h_dofmap, &
              w0_orientation, w1_orientation, w2_orientation, w3_orientation, &
              wtheta_orientation, w2v_orientation, w2h_orientation, &
              w0_global_dof_id, w0_last_dof_owned, w0_last_dof_annexed, &
              w0_last_dof_halo, &
              w1_global_dof_id, w1_last_dof_owned, w1_last_dof_annexed, &
              w1_last_dof_halo, &
              w2_global_dof_id, w2_last_dof_owned, w2_last_dof_annexed, &
              w2_last_dof_halo, &
              w3_global_dof_id, w3_last_dof_owned, w3_last_dof_annexed, &
              w3_last_dof_halo,&
              wtheta_global_dof_id, wtheta_last_dof_owned, &
              wtheta_last_dof_annexed, wtheta_last_dof_halo, &
              w2v_global_dof_id, w2v_last_dof_owned, &
              w2v_last_dof_annexed, w2v_last_dof_halo, &
              w2h_global_dof_id, w2h_last_dof_owned, &
              w2h_last_dof_annexed, w2h_last_dof_halo


    use mesh_mod,  only: mesh_type
    use slush_mod, only: w_unique_dofs
    
    implicit none


    type (mesh_type), intent(in) :: mesh
    integer          :: scalar, vector
    integer          :: nqp_h, nqp_v
    real(kind=r_def), pointer :: xp(:,:) => null()
    real(kind=r_def), pointer :: zp(:) => null()
    
    integer :: undf_l, ndf_l
    integer :: W0_l, W1_l, W2_l, W3_l, Wtheta_l, W2V_l, W2H_l
    integer :: num_cells

    undf_l = 1
    ndf_l  = 2

    W0_l = W0 - W0 + 1
    W1_l = W1 - W0 + 1
    W2_l = W2 - W0 + 1
    W3_l = W3 - W0 + 1
    Wtheta_l = Wtheta - W0 + 1
    W2V_l = W2V - W0 + 1
    W2H_l = W2H - W0 + 1

    scalar=1
    vector=3

    num_cells = mesh%get_ncells_2d()

    ! Quads at lowest order
    ! w0
    w_unique_dofs(1,1) = 36
    w_unique_dofs(1,2) = 8

    ! w1
    w_unique_dofs(2,1) = 99
    w_unique_dofs(2,2) = 12

    ! w2
    w_unique_dofs(3,1) = 90
    w_unique_dofs(3,2)  = 6

    ! w3
    w_unique_dofs(4,1) = 27
    w_unique_dofs(4,2)  = 1

    !wtheta
    w_unique_dofs(5,1) = 36
    w_unique_dofs(5,2)  = 2

    !w2v
    w_unique_dofs(6,1) = 36
    w_unique_dofs(6,2)  = 2

    !w2h
    w_unique_dofs(7,1) = 54
    w_unique_dofs(7,2)  = 4

    
    if (.not.allocated(w0_nodal_coords))                                       &
       allocate(w0_nodal_coords(3,w_unique_dofs(1,2)))
    if (.not.allocated(w1_nodal_coords))                                       &
       allocate(w1_nodal_coords(3,w_unique_dofs(2,2)))
    if (.not.allocated(w2_nodal_coords))                                       &
       allocate(w2_nodal_coords(3,w_unique_dofs(3,2)))
    if (.not.allocated(w3_nodal_coords))                                       &
       allocate(w3_nodal_coords(3,w_unique_dofs(4,2)))
    if(.not.allocated(wtheta_nodal_coords) )                                   &
         allocate(wtheta_nodal_coords(3,w_unique_dofs(5,2)))
    if(.not.allocated(w2v_nodal_coords) )                                      &
         allocate(w2v_nodal_coords(3,w_unique_dofs(6,2)))
    if(.not.allocated(w2h_nodal_coords) )                                      &
         allocate(w2h_nodal_coords(3,w_unique_dofs(7,2)))
    if (.not.allocated(w0_dofmap))                                             &
       allocate( w0_dofmap(w_unique_dofs(1,2),1:num_cells) )
    if (.not.allocated(w1_dofmap))                                             &
       allocate( w1_dofmap(w_unique_dofs(2,2),1:num_cells) )
    if (.not.allocated(w2_dofmap))                                             &
       allocate( w2_dofmap(w_unique_dofs(3,2),1:num_cells) )
    if (.not.allocated(w3_dofmap))                                             &
       allocate( w3_dofmap(w_unique_dofs(4,2),1:num_cells) )
    if(.not.allocated(wtheta_dofmap) )                                         &
         allocate( wtheta_dofmap(w_unique_dofs(5,2),1:num_cells) )
    if(.not.allocated(w2v_dofmap) )                                            &
         allocate( w2v_dofmap(w_unique_dofs(6,2),1:num_cells) )
    if(.not.allocated(w2h_dofmap) )                                            &
         allocate( w2h_dofmap(w_unique_dofs(7,2),1:num_cells) ) 
    if (.not.allocated(test_map_w0))                                           &
       allocate( test_map_w0(w_unique_dofs(1,2),1:num_cells) )
    if (.not.allocated(test_map_w1))                                           &
       allocate( test_map_w1(w_unique_dofs(2,2),1:num_cells) )
    if (.not.allocated(test_map_w2))                                           &
       allocate( test_map_w2(w_unique_dofs(3,2),1:num_cells) )
    if (.not.allocated(test_map_w3))                                           &
       allocate( test_map_w3(w_unique_dofs(4,2),1:num_cells) )
    if(.not.allocated(test_map_wtheta) )                                       &
         allocate( test_map_wtheta(w_unique_dofs(5,2),1:num_cells) )
    if(.not.allocated(test_map_w2v) )                                          &
         allocate( test_map_w2v(w_unique_dofs(6,2),1:num_cells) )
    if(.not.allocated(test_map_w2h) )                                          &
         allocate( test_map_w2h(w_unique_dofs(7,2),1:num_cells) )
       
    if(.not.allocated( w0_orientation) ) then  ! reasonable assumption!
       allocate( w0_orientation(num_cells, w_unique_dofs(1,2) ))
       allocate( w0_basis_index(3,w_unique_dofs(1,2)) )
       allocate( w0_basis_order(3,w_unique_dofs(1,2)) )
       allocate( w0_basis_vector(1,w_unique_dofs(1,2)) )
       allocate( w0_basis_x(2,3,w_unique_dofs(1,2)) )  ! lowest order k+2:3vec:ndf
       allocate( w0_dof_on_vert_boundary(w_unique_dofs(1,2),2) )
    end if
    
    if(.not.allocated( w1_orientation) ) then  ! reasonable assumption!
       allocate( w1_orientation(num_cells, w_unique_dofs(2,2) ))
       allocate( w1_basis_index(3,w_unique_dofs(2,2)) )
       allocate( w1_basis_order(3,w_unique_dofs(2,2)) )
       allocate( w1_basis_vector(3,w_unique_dofs(2,2)) )
       allocate( w1_basis_x(2,3,w_unique_dofs(2,2)) )  ! lowest order k+2:3vec:ndf
       allocate( w1_dof_on_vert_boundary(w_unique_dofs(2,2),2) )
    end if
       
    if(.not.allocated( w2_orientation) ) then  ! reasonable assumption!       
       allocate( w2_orientation(num_cells, w_unique_dofs(3,2) ))
       allocate( w2_basis_index(3,w_unique_dofs(3,2)) )
       allocate( w2_basis_order(3,w_unique_dofs(3,2)) )
       allocate( w2_basis_vector(3,w_unique_dofs(3,2)) )
       allocate( w2_basis_x(2,3,w_unique_dofs(3,2)) )  ! lowest order k+2:3vec:ndf
       allocate( w2_dof_on_vert_boundary(w_unique_dofs(3,2),2) )
    end if
       
    if(.not.allocated( w3_orientation) ) then  ! reasonable assumption!
       allocate( w3_orientation(num_cells, w_unique_dofs(4,2) ))
       allocate( w3_basis_index(3,w_unique_dofs(4,2)) )
       allocate( w3_basis_order(3,w_unique_dofs(4,2)) )
       allocate( w3_basis_vector(1,w_unique_dofs(4,2)) )
       allocate( w3_basis_x(2,3,w_unique_dofs(4,2)) )  ! lowest order k+2:3vec:ndf
       allocate( w3_dof_on_vert_boundary(w_unique_dofs(4,2),2) )  
    end if

    if(.not.allocated( wtheta_orientation) ) then  ! reasonable assumption!       
       allocate( wtheta_orientation(num_cells, w_unique_dofs(5,2) ))
       allocate( wtheta_basis_index(3,w_unique_dofs(5,2)) )
       allocate( wtheta_basis_order(3,w_unique_dofs(5,2)) )
       allocate( wtheta_basis_vector(1,w_unique_dofs(5,2)) )
       allocate( wtheta_basis_x(2,3,w_unique_dofs(5,2)) )  ! lowest order k+2:3vec:ndf
       allocate( wtheta_dof_on_vert_boundary(w_unique_dofs(5,2),2) )
    end if

    if(.not.allocated( w2v_orientation) ) then  ! reasonable assumption!       
       allocate( w2v_orientation(num_cells, w_unique_dofs(6,2) ))
       allocate( w2v_basis_index(3,w_unique_dofs(6,2)) )
       allocate( w2v_basis_order(3,w_unique_dofs(6,2)) )
       allocate( w2v_basis_vector(3,w_unique_dofs(6,2)) )
       allocate( w2v_basis_x(2,3,w_unique_dofs(6,2)) )  ! lowest order k+2:3vec:ndf
       allocate( w2v_dof_on_vert_boundary(w_unique_dofs(6,2),2) )
    end if

    if(.not.allocated( w2h_orientation) ) then  ! reasonable assumption!       
       allocate( w2h_orientation(num_cells, w_unique_dofs(7,2) ))
       allocate( w2h_basis_index(3,w_unique_dofs(7,2)) )
       allocate( w2h_basis_order(3,w_unique_dofs(7,2)) )
       allocate( w2h_basis_vector(3,w_unique_dofs(7,2)) )
       allocate( w2h_basis_x(2,3,w_unique_dofs(7,2)) )  ! lowest order k+2:3vec:ndf
       allocate( w2h_dof_on_vert_boundary(w_unique_dofs(7,2),2) )
    end if

    if(.not.allocated( w0_global_dof_id) ) &
       allocate( w0_global_dof_id(w_unique_dofs(1,1)) )
    if(.not.allocated( w1_global_dof_id) ) &
       allocate( w1_global_dof_id(w_unique_dofs(2,1)) )
    if(.not.allocated( w2_global_dof_id) ) &
       allocate( w2_global_dof_id(w_unique_dofs(3,1)) )
    if(.not.allocated( w3_global_dof_id) ) &
       allocate( w3_global_dof_id(w_unique_dofs(4,1)) )
    if(.not.allocated( wtheta_global_dof_id) ) &
       allocate( wtheta_global_dof_id(w_unique_dofs(5,1)) )
    if(.not.allocated( w2v_global_dof_id) ) &
       allocate( w2v_global_dof_id(w_unique_dofs(6,1)) )
    if(.not.allocated( w2h_global_dof_id) ) &
       allocate( w2h_global_dof_id(w_unique_dofs(7,1)) )

    if(.not.allocated( w0_last_dof_halo) ) &
       allocate( w0_last_dof_halo( 1 ) )
    if(.not.allocated( w1_last_dof_halo) ) &
       allocate( w1_last_dof_halo( 1 ) )
    if(.not.allocated( w2_last_dof_halo) ) &
       allocate( w2_last_dof_halo( 1 ) )
    if(.not.allocated( w3_last_dof_halo) ) &
       allocate( w3_last_dof_halo( 1 ) )
    if(.not.allocated( wtheta_last_dof_halo) ) &
       allocate( wtheta_last_dof_halo( 1 ) )
    if(.not.allocated( w2h_last_dof_halo) ) &
       allocate( w2h_last_dof_halo( 1 ) )
    if(.not.allocated( w2v_last_dof_halo) ) &
       allocate( w2v_last_dof_halo( 1 ) )

    ! make some the w0 space
    w0_dofmap =  reshape( [ &
         1, 5, 9,13, 2, 6,10,14, &
         5,17,21, 9, 6,18,22,10,  &
         17,1,13,21,18, 2,14,22,  &
         13,9,25,29,14,10,26,30,  &
         9,21,33,25,10,22,34,26,  &
         21,13,29,33,22,14,30,34, &
         29,25, 5, 1,30,26, 6, 2, &
         25,33,17, 5,26,34,18, 6, &
         33,29, 1,17,34,30, 2,18  &
         ], shape(w0_dofmap) ) 

    test_map_w0 = w0_dofmap

    w0_orientation = reshape( [ &
         1, 1, 1, 1, 1, 1, 1, 1, 1,         &
         1, 1, 1, 1, 1, 1, 1, 1, 1,         &
         1, 1, 1, 1, 1, 1, 1, 1, 1,         &
         1, 1, 1, 1, 1, 1, 1, 1, 1,         &
         1, 1, 1, 1, 1, 1, 1, 1, 1,         &
         1, 1, 1, 1, 1, 1, 1, 1, 1,         &
         1, 1, 1, 1, 1, 1, 1, 1, 1,         &
         1, 1, 1, 1, 1, 1, 1, 1, 1          &
         ], shape(w0_orientation) ) 

    w0_basis_index = reshape( [ &
         1, 1, 1,                           &
         2, 1, 1,                           &
         2, 2, 1,                           &
         1, 2, 1,                           &
         1, 1, 2,                           &
         2, 1, 2,                           &
         2, 2, 2,                           &
         1, 2, 2                            &
         ], shape(w0_basis_index) ) 

    w0_basis_order = reshape( [ &
         1, 1, 1,                           &
         1, 1, 1,                           &
         1, 1, 1,                           &
         1, 1, 1,                           &
         1, 1, 1,                           &
         1, 1, 1,                           &
         1, 1, 1,                           &
         1, 1, 1                            &
         ], shape(w0_basis_order) ) 

    w0_basis_vector = reshape( [ &
         0.1000000000000000E+01_r_def,             &
         0.1000000000000000E+01_r_def,             &
         0.1000000000000000E+01_r_def,             &
         0.1000000000000000E+01_r_def,             &
         0.1000000000000000E+01_r_def,             &
         0.1000000000000000E+01_r_def,             &
         0.1000000000000000E+01_r_def,             &
         0.1000000000000000E+01_r_def              &
         ], shape(w0_basis_vector) ) 

    w0_basis_x =  reshape( [      &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def  &
         ], shape(w0_basis_x) ) 

    w0_nodal_coords = reshape( [ &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.1000000000000000E+01_r_def, 0.1000000000000000E+01_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.1000000000000000E+01_r_def  &
         ], shape(w0_nodal_coords) )

    w0_dof_on_vert_boundary(:,:) = 1

    w0_global_dof_id = [ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, &
                        13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, &
                        25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36 ]

    w0_last_dof_owned = 36
    w0_last_dof_annexed = 36
    w0_last_dof_halo(1) = 36

    w0_func_space = w0_func_space%get_instance( mesh, W0 )

   ! w1 space

    w1_dofmap =  reshape( [ &
         1, 5, 9,13,17,20,23,26, 2, 6,10,14,  & 
         29,33,37, 5,20,41,44,23,30,34,38, 6, &
         47,13,51,33,41,17,26,44,48,14,52,34, &
         9,55,59,63,26,23,67,70,10,56,60,64,  &
         37,73,77,55,23,44,81,67,38,74,78,56, &
         51,63,84,73,44,26,70,81,52,64,85,74, &
         59,88, 1,92,70,67,20,17,60,89, 2,93, &
         77,96,29,88,67,81,41,20,78,97,30,89, &
         84,92,47,96,81,70,17,41,85,93,48,97  &
         ], shape(w1_dofmap) )

    test_map_w1 = w1_dofmap    

    w1_orientation = reshape( [ &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, & 
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1  &
         ], shape(w1_orientation) )
    
    w1_basis_index = reshape( [ &
         1, 1, 1, &
         1, 1, 1, &
         2, 1, 1, &
         1, 2, 1, &
         1, 1, 1, &
         2, 1, 1, &
         2, 2, 1, &
         1, 2, 1, &
         1, 1, 2, &
         1, 1, 2, &
         2, 1, 2, &
         1, 2, 2  &
         ], shape(w1_basis_index) )
    
    
    w1_basis_order = reshape( [ &
         1, 0, 1, &
         0, 1, 1, &
         1, 0, 1, &
         0, 1, 1, &
         1, 1, 0, &
         1, 1, 0, &
         1, 1, 0, &
         1, 1, 0, &
         1, 0, 1, &
         0, 1, 1, &
         1, 0, 1, &
         0, 1, 1  &       
         ], shape(w1_basis_order) )

    w1_basis_vector = reshape( [ &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def  &
         ], shape(w1_basis_vector) )

    w1_basis_x =  reshape( [      &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def  &
         ], shape(w1_basis_x) )

    w1_nodal_coords = reshape( [ &
         0.00000000E+00_r_def, 0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, 0.00000000E+00_r_def, &
         0.10000000E+01_r_def, 0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.10000000E+01_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.00000000E+00_r_def, 0.50000000E+00_r_def, &
         0.10000000E+01_r_def, 0.00000000E+00_r_def, 0.50000000E+00_r_def, &
         0.10000000E+01_r_def, 0.10000000E+01_r_def, 0.50000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, 0.50000000E+00_r_def, &
         0.00000000E+00_r_def, 0.50000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.10000000E+01_r_def, 0.50000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.10000000E+01_r_def, 0.10000000E+01_r_def  &
         ], shape(w1_nodal_coords) )

    w1_dof_on_vert_boundary(:,:) = 1
    w1_dof_on_vert_boundary(1:4,1) = 0
    w1_dof_on_vert_boundary(9:12,2) = 0

    w1_global_dof_id = [ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, &
                        13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, &
                        25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, &
                        37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, &
                        49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, &
                        61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, &
                        73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, &
                        85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, &
                        97, 98, 99 ]

    w1_last_dof_owned = 99
    w1_last_dof_annexed = 99
    w1_last_dof_halo(1) = 99

    w1_func_space = w1_func_space%get_instance( mesh, W1 )

   ! w2 space

    w2_dofmap =  reshape( [ &
         1, 4, 7,10,13,14,  &
         17,20,23, 4,26,27, &
         30,10,33,20,36,37, &
         7,40,43,46,49,50,  &
         23,53,56,40,59,60, &
         33,46,63,53,66,67, &
         43,70, 1,73,76,77, &
         56,80,17,70,83,84, &
         63,73,30,80,87,88  &
         ], shape(w2_dofmap) )

    test_map_w2 = w2_dofmap    

    w2_orientation = reshape( [ &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1  &
         ], shape(w2_orientation) )

    w2_basis_index = reshape( [ &         
         1, 1, 1, &
         1, 1, 1, &
         2, 1, 1, &
         1, 2, 1, &
         1, 1, 1, &
         1, 1, 2  &
         ], shape(w2_basis_index) )

    w2_basis_order = reshape( [ &
         1, 0, 0, &
         0, 1, 0, &
         1, 0, 0, &
         0, 1, 0, &
         0, 0, 1, &
         0, 0, 1  &
         ], shape(w2_basis_order) )

    w2_basis_vector = reshape( [ &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def  &
         ], shape(w2_basis_vector) )

    w2_basis_x = reshape( [ &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def  &
         ], shape(w2_basis_x) )

    w2_nodal_coords = reshape( [ &
         0.0000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.5000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.1000000000000000E+01_r_def  &
         ], shape(w2_nodal_coords) )

    w2_dof_on_vert_boundary(:,:) = 1
    w2_dof_on_vert_boundary(5,1) = 0
    w2_dof_on_vert_boundary(6,2) = 0

    w2_global_dof_id = [ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, &
                        13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, &
                        25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, &
                        37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, &
                        49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, &
                        61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, &
                        73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, &
                        85, 86, 87, 88, 89, 90 ]

    w2_last_dof_owned = 90
    w2_last_dof_annexed = 90
    w2_last_dof_halo(1) = 90

    w2_func_space = w2_func_space%get_instance( mesh, W2 )

   ! w3 space

    w3_dofmap =  reshape( [ &
    1,  &
    4,  &
    7,  &
    10, &
    13, &
    16, &
    19, &
    22, &
    25  &
    ], shape(w3_dofmap) )

    test_map_w3 = w3_dofmap
    
    w3_orientation = reshape( [ &
         1, 1, 1, 1, 1, 1, 1, 1, 1 &
         ], shape(w3_orientation) )
    
    w3_basis_index = reshape( [ &
         1, 1, 1 &
         ], shape(w3_basis_index) )
         
    w3_basis_order = reshape( [ &
         0, 0, 0  &
         ], shape(w3_basis_order) )

    w3_basis_vector = reshape( [ &
         0.1000000000000000E+01_r_def &
         ], shape(w3_basis_vector) )

    w3_basis_x = reshape( [ &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def  &
         ], shape(w3_basis_x) )

    w3_nodal_coords = reshape( [ &
         0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def &
         ], shape(w3_nodal_coords) )

    w3_dof_on_vert_boundary(:,:) = 1         

    w3_global_dof_id = [ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, &
                        13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, &
                        25, 26, 27 ]

    w3_last_dof_owned = 27
    w3_last_dof_annexed = 27
    w3_last_dof_halo(1) = 27

    w3_func_space = w3_func_space%get_instance( mesh, W3 )

    ! wtheta space

    wtheta_dofmap =  reshape( [ &
         1, 2,  &
         5, 6,  &
         9,10,  &
         13,14, &
         17,18, &
         21,22, &
         25,26, &
         29,30, &
         33,34  &
         ], shape(wtheta_dofmap) )

    test_map_wtheta = wtheta_dofmap

    wtheta_orientation = reshape( [ &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1  &
         ], shape(wtheta_orientation) )

    wtheta_basis_index = reshape( [ &
         1, 1, 1, &
         1, 1, 2  &
         ], shape(wtheta_basis_index) )

    wtheta_basis_order = reshape( [ &
         0, 0, 1, &
         0, 0, 1  &
         ], shape(wtheta_basis_order) )

    wtheta_basis_vector = reshape( [ &
         0.1000000000000000E+01_r_def, 0.1000000000000000E+01_r_def &
         ], shape(wtheta_basis_vector) )

    wtheta_basis_x = reshape( [ &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def  &
         ], shape(wtheta_basis_x) )

    wtheta_nodal_coords = reshape( [ &
         0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def,&
         0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.1000000000000000E+01_r_def &
         ], shape(wtheta_nodal_coords) )

    wtheta_dof_on_vert_boundary(:,:) = 1
    wtheta_dof_on_vert_boundary(1,1) = 0
    wtheta_dof_on_vert_boundary(2,2) = 0

    wtheta_global_dof_id = [ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, &
                            13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, &
                            25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36 ]

    wtheta_last_dof_owned = 36
    wtheta_last_dof_annexed = 36
    wtheta_last_dof_halo(1) = 36

    wtheta_func_space = wtheta_func_space%get_instance( mesh, Wtheta )

    ! w2v space
    w2v_dofmap =  reshape( [ &
         1, 2,  &
         5, 6,  &
         9,10,  &
         13,14, &
         17,18, &
         21,22, &
         25,26, &
         29,30, &
         33,34  &
         ], shape(w2v_dofmap) )

    test_map_w2v = w2v_dofmap

    w2v_orientation = reshape( [ &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1  &
         ], shape(w2v_orientation) )

    w2v_basis_index = reshape( [ &
         1, 1, 1, &
         1, 1, 2  &
         ], shape(w2v_basis_index) )

    w2v_basis_order = reshape( [ &
         0, 0, 1, &
         0, 0, 1  &
         ], shape(w2v_basis_order) )

    w2v_basis_vector = reshape( [ &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def  &
         ], shape(w2v_basis_vector) )

    w2v_basis_x = reshape( [ &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def  &
         ], shape(w2v_basis_x) )

    w2v_nodal_coords = reshape( [ &
         0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.1000000000000000E+01_r_def  &
         ], shape(w2v_nodal_coords) )

    w2v_dof_on_vert_boundary(:,:) = 1
    w2v_dof_on_vert_boundary(1,1) = 0
    w2v_dof_on_vert_boundary(2,2) = 0

    w2v_global_dof_id = [ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, &
                         13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, &
                         25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36 ]

    w2v_last_dof_owned = 36
    w2v_last_dof_annexed = 36
    w2v_last_dof_halo(1) = 36

    w2v_func_space = w2v_func_space%get_instance( mesh, W2V )

   ! w2h space

    w2h_dofmap =  reshape( [ &
         1,  4,  7,  10, &
         13, 16, 19, 4,  &
         22, 10, 25, 16, &
         7,  28, 31, 34, &
         19, 37, 40, 28, &
         25, 34, 43, 37, &
         31, 46, 1,  49, &
         40, 52, 13, 46, &
         43, 49, 22, 52  &
         ], shape(w2h_dofmap) )

    test_map_w2h = w2h_dofmap    

    w2h_orientation = reshape( [ &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 1, 1, 1, 1  &
         ], shape(w2h_orientation) )

    w2h_basis_index = reshape( [ &         
         1, 1, 1, &
         1, 1, 1, &
         2, 1, 1, &
         1, 2, 1  &
         ], shape(w2h_basis_index) )

    w2h_basis_order = reshape( [ &
         1, 0, 0, &
         0, 1, 0, &
         1, 0, 0, &
         0, 1, 0  &
         ], shape(w2h_basis_order) )

    w2h_basis_vector = reshape( [ &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, &
         0.0000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.0000000000000000E+00_r_def  &
         ], shape(w2h_basis_vector) )

    w2h_basis_x = reshape( [ &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def, &
         0.00000000E+00_r_def, 0.10000000E+01_r_def, &
         0.50000000E+00_r_def, 0.00000000E+00_r_def  &
         ], shape(w2h_basis_x) )

    w2h_nodal_coords = reshape( [ &
         0.0000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.0000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, &
         0.1000000000000000E+01_r_def, 0.5000000000000000E+00_r_def, 0.5000000000000000E+00_r_def, &
         0.5000000000000000E+00_r_def, 0.1000000000000000E+01_r_def, 0.5000000000000000E+00_r_def  &
         ], shape(w2h_nodal_coords) )

    w2h_dof_on_vert_boundary(:,:) = 1

    w2h_global_dof_id = [ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, &
                         13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, &
                         25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, &
                         37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, &
                         49, 50, 51, 52, 53, 54 ]

    w2h_last_dof_owned = 54
    w2h_last_dof_annexed = 54
    w2h_last_dof_halo(1) = 54

    w2h_func_space = w2h_func_space%get_instance( mesh, W2H )

  end subroutine fs_build

  subroutine get_test_map(test_map,fs)
    implicit none
    integer, intent(in) :: fs
    integer, pointer, dimension(:,:), intent(out) :: test_map 

    select case ( fs )
       case( W0) 
          test_map => test_map_w0
       case( W1) 
          test_map => test_map_w1
       case( W2) 
          test_map => test_map_w2
       case( W3) 
          test_map => test_map_w3
       case( Wtheta)
          test_map => test_map_wtheta
       case( W2V)
          test_map => test_map_w2v
       case( W2H)
          test_map => test_map_w2h
       case default
          test_map => null()
     end select

  end subroutine get_test_map

  subroutine basis_func_w0( basis )
    implicit none
    real(kind=r_def), dimension(1, 8, 9, 3), intent(out) :: basis 
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w0_basis.dat"
        ], shape(basis) )
    
    return
  end subroutine basis_func_w0

  subroutine diff_basis_func_w0( basis )
    implicit none
    real(kind=r_def), dimension(3, 8, 9, 3), intent(out) :: basis 
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w0_diff.dat"
        ], shape(basis) )

    return
  end subroutine diff_basis_func_w0

  subroutine basis_func_w1( basis )
    implicit none
    real(kind=r_def), dimension(3, 12, 9, 3), intent(out) :: basis 
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w1_basis.dat"
          ], shape(basis) )
    return
  end subroutine basis_func_w1

  subroutine diff_basis_func_w1( basis )
    implicit none
    real(kind=r_def), dimension(3, 12 , 9, 3), intent(out) :: basis 
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w1_diff.dat"
        ], shape(basis) )
    return
  end subroutine diff_basis_func_w1

  subroutine basis_func_w2( basis )
    implicit none
    real(kind=r_def), dimension(3, 6, 9, 3), intent(out) :: basis 
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w2_basis.dat"
          ], shape(basis) )
    return
  end subroutine basis_func_w2

  subroutine diff_basis_func_w2( basis )
    implicit none
    real(kind=r_def), dimension(1, 6, 9, 3), intent(out) :: basis 
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w2_diff.dat"
          ], shape(basis) )
    return
  end subroutine diff_basis_func_w2

  subroutine basis_func_w3( basis )
    implicit none
    real(kind=r_def), dimension(1, 1, 9, 3), intent(out) :: basis 
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w3_basis.dat"
          ], shape(basis) )
    return
  end subroutine basis_func_w3

  subroutine diff_basis_func_w3( basis )
    implicit none
    real(kind=r_def), dimension(1, 1, 9, 3), intent(out) :: basis 
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w3_diff.dat"
          ], shape(basis) )
    return
  end subroutine diff_basis_func_w3

  subroutine basis_func_wtheta( basis )
    implicit none
    real(kind=r_def), dimension(1, 2, 9, 3), intent(out) :: basis
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/wtheta_basis.dat"
          ], shape(basis) )
    return
  end subroutine basis_func_wtheta

  subroutine diff_basis_func_wtheta( basis )
    implicit none
    real(kind=r_def), dimension(3, 2, 9, 3), intent(out) :: basis
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/wtheta_diff.dat"
          ], shape(basis) )
    return
  end subroutine diff_basis_func_wtheta

  subroutine basis_func_w2v( basis )
    implicit none
    real(kind=r_def), dimension(3, 2, 9, 3), intent(out) :: basis
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w2v_basis.dat"
          ], shape(basis) )
    return
  end subroutine basis_func_w2v

  subroutine diff_basis_func_w2v( basis )
    implicit none
    real(kind=r_def), dimension(1, 2, 9, 3), intent(out) :: basis
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w2v_diff.dat"
          ], shape(basis) )
    return
  end subroutine diff_basis_func_w2v

  subroutine basis_func_w2h( basis )
    implicit none
    real(kind=r_def), dimension(3, 4, 9, 3), intent(out) :: basis
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w2h_basis.dat"
          ], shape(basis) )
    return
  end subroutine basis_func_w2h

  subroutine diff_basis_func_w2h( basis )
    implicit none
    real(kind=r_def), dimension(1, 4, 9, 3), intent(out) :: basis
    ! hard-coded array bounds. This is static

    basis = reshape ( [ &
#include "../data/w2h_diff.dat"
          ], shape(basis) )
    return
  end subroutine diff_basis_func_w2h

  subroutine fs_destroy()
    implicit none
    if (allocated(test_map_w0) ) then
       deallocate(test_map_w0)
    end if
    if (allocated(test_map_w1) ) then
       deallocate(test_map_w1)
    end if
    if (allocated(test_map_w2) ) then
       deallocate(test_map_w2)
    end if
    if (allocated(test_map_w3) ) then
       deallocate(test_map_w3)
    end if 
    if (allocated(test_map_wtheta) ) then
       deallocate(test_map_wtheta)
    end if
    if (allocated(test_map_w2v) ) then
       deallocate(test_map_w2v)
    end if
    if (allocated(test_map_w2h) ) then
       deallocate(test_map_w2h)
    end if

    return
  end subroutine fs_destroy

end module function_space_build_mod

