!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel computes the initial theta field

!> @detail The kernel computes initial theta perturbation field for theta in the space
!>         of horizontally discontinuous, vertically continuous polynomials

module initial_theta_wtheta_kernel_mod

    use argument_mod,                  only: arg_type,  &
        GH_FIELD, GH_WRITE, GH_READ,                    &
        W0, Wtheta, GH_BASIS,                      &
        GH_DIFF_BASIS,                                  &
        CELLS
    use base_mesh_config_mod,          only: geometry, &
        base_mesh_geometry_spherical
    use constants_mod,                 only: r_def, i_def, PI
    use coord_transform_mod,           only: xyz2llr
    use formulation_config_mod,        only: nonlinear
    use generate_global_gw_fields_mod, only: generate_global_gw_pert
    use idealised_config_mod,          only: test,              &
        idealised_test_gravity_wave, &
        idealised_test_cold_bubble,  &
        idealised_test_warm_bubble

    use kernel_mod,                    only: kernel_type
    use planet_config_mod,             only: scaled_radius
    use reference_profile_mod,         only: reference_profile

    implicit none

    !-------------------------------------------------------------------------------
    ! Public types
    !-------------------------------------------------------------------------------
    !> The type declaration for the kernel. Contains the metadata needed by the Psy layer
    type, public, extends(kernel_type) :: initial_theta_wtheta_kernel_type
        private
        type(arg_type) :: meta_args(2) = (/                               &
            arg_type(GH_FIELD,   GH_WRITE, Wtheta),                  &
            arg_type(GH_FIELD*3, GH_READ, W0)                             &
            /)
        integer :: iterates_over = CELLS

    contains
        procedure, nopass :: initial_theta_wtheta_code
    end type

    !-------------------------------------------------------------------------------
    ! Constructors
    !-------------------------------------------------------------------------------

    ! overload the default structure constructor for function space
    interface initial_theta_wtheta_kernel_type
        module procedure initial_theta_wtheta_kernel_constructor
    end interface

    !-------------------------------------------------------------------------------
    ! Contained functions/subroutines
    !-------------------------------------------------------------------------------
    public initial_theta_wtheta_code
contains

    type(initial_theta_wtheta_kernel_type) function initial_theta_wtheta_kernel_constructor() result(self)
        return
    end function initial_theta_wtheta_kernel_constructor

      !> @brief The subroutine which is called directly by the Psy layer
      !! @param[in] nlayers Integer the number of layers
      !! @param[in] ndf_wtheta The number of degrees of freedom per cell for wtheta
      !! @param[in] udf_wtheta The number of total degrees of freedom for wtheta
      !! @param[in] map_wtheta Integer array holding the dofmap for the cell at the base of the column
      !! @param[inout] theta Real array the data
      !! @param[in] wtheta_basis Real 5-dim array holding basis functions evaluated at gaussian quadrature points
      !! @param[in] ndf_w0 The number of degrees of freedom per cell
      !! @param[in] ndf_w0 The total number of degrees of freedom
      !! @param[in] map_w0 Integer array holding the dofmap for the cell at the base of the column
      !! @param[in] w0_basis Real 5-dim array holding basis functions evaluated at gaussian quadrature points
      !! @param[inout] chi_1 Real array, the x component of the w0 coordinate field
      !! @param[inout] chi_2 Real array, the y component of the w0 coordinate field
      !! @param[inout] chi_3 Real array, the z component of the w0 coordinate field

      ! In Psyclone

    subroutine initial_theta_wtheta_code(cell, nlayers, ndf_wtheta, undf_wtheta, map_wtheta, theta, &
        ndf_w0, undf_w0, map_w0, w0_basis, chi_1, chi_2, chi_3)

        use matrix_invert_mod,       only : matrix_invert
        use coordinate_jacobian_mod, only : coordinate_jacobian

        !Arguments
        integer(kind=i_def), intent(in) :: cell, nlayers, ndf_wtheta, ndf_w0, undf_wtheta, undf_w0
        integer(kind=i_def), dimension(ndf_wtheta), intent(in) :: map_wtheta
        integer(kind=i_def), dimension(ndf_w0), intent(in) :: map_w0
        real(kind=r_def), dimension(undf_wtheta),          intent(inout) :: theta
        real(kind=r_def), dimension(undf_w0),              intent(in)    :: chi_1, chi_2, chi_3
        real(kind=r_def), dimension(1,ndf_w0,ndf_wtheta),  intent(in)    :: w0_basis

        !Internal variables
        integer(kind=i_def)              :: df, df0, k

        real(kind=r_def), dimension(ndf_w0)             :: chi_1_e, chi_2_e, chi_3_e

        real(kind=r_def), parameter :: THETA0 = 0.01_r_def
        real(kind=r_def), parameter :: XC     = -15000.0_r_def
        real(kind=r_def), parameter :: YC     = 0.0_r_def
        real(kind=r_def), parameter :: A      = 2500.0_r_def
        real(kind=r_def), parameter :: H      = 10000.0_r_def
        real(kind=r_def)            :: x(3)
        real(kind=r_def)            :: theta_ref, exner_ref, rho_ref

        real(kind=r_def)            :: lat, lon, r
        real(kind=r_def)            :: theta_pert, nl
        real(kind=r_def)            :: l, dt
        real(kind=r_def), parameter :: XR = 4000.0_r_def, &
            ZC_cold = 3000.0_r_def, &
            ZC_hot = 260.0_r_def, &
            ZR = 2000.0_r_def

        ! compute the pointwise theta profile
        if ( nonlinear ) then
            nl = 1.0
        else
            nl = 0.0
        end if

        if ( geometry == base_mesh_geometry_spherical ) then

            do k = 0, nlayers-1
                do df0 = 1, ndf_w0
                    chi_1_e(df0) = chi_1( map_w0(df0) + k)
                    chi_2_e(df0) = chi_2( map_w0(df0) + k)
                    chi_3_e(df0) = chi_3( map_w0(df0) + k)
                end do

                do df = 1, ndf_wtheta
                    x(:) = 0.0_r_def
                    do df0 = 1, ndf_w0
                        x(1) = x(1) + chi_1_e(df0)*w0_basis(1,df0,df)
                        x(2) = x(2) + chi_2_e(df0)*w0_basis(1,df0,df)
                        x(3) = x(3) + chi_3_e(df0)*w0_basis(1,df0,df)
                    end do
                    call reference_profile(exner_ref, rho_ref, theta_ref, x, test)

                    call xyz2llr(x(1), x(2), x(3), lon, lat, r)
                    theta_pert = generate_global_gw_pert(lon,lat,r-scaled_radius)

                    theta(map_wtheta(df) + k) = nl*theta_ref + theta_pert
                end do

            end do

        else

            ! compute initial theta
            do k = 0, nlayers-1
                do df0 = 1, ndf_w0
                    chi_1_e(df0) = chi_1( map_w0(df0) + k)
                    chi_2_e(df0) = chi_2( map_w0(df0) + k)
                    chi_3_e(df0) = chi_3( map_w0(df0) + k)
                end do

                do df = 1, ndf_wtheta
                    x(:) = 0.0_r_def
                    do df0 = 1, ndf_w0
                        x(1) = x(1) + chi_1_e(df0)*w0_basis(1,df0,df)
                        x(2) = x(2) + chi_2_e(df0)*w0_basis(1,df0,df)
                        x(3) = x(3) + chi_3_e(df0)*w0_basis(1,df0,df)
                    end do

                    call reference_profile(exner_ref, rho_ref, theta_ref, x, test)
                    select case( test )
                        case( idealised_test_gravity_wave )
                          theta_pert = THETA0 * sin ( PI * x(3) / H ) &
                                                / ( 1.0_r_def + ( x(1) - XC )**2/A**2 )
                        case ( idealised_test_cold_bubble )
                          l = sqrt( ((x(1)-XC)/XR)**2 + ((x(3)-ZC_cold)/ZR)**2 )
                          if ( l <= 1.0_r_def ) then
                            dt =  15.0_r_def/2.0_r_def*(cos(PI*l)+1.0_r_def)
                            theta_pert = - dt/exner_ref
                          end if
                        case( idealised_test_warm_bubble )   ! Warm bubble test
                          l = sqrt( ((x(1)-XC))**2 + ((x(3)-ZC_hot))**2 )
                          if ( l <= 50.0_r_def ) then
                            dt = 0.5_r_def
                          else
                            dt = 0.5_r_def*exp(-(l-50.0_r_def)**2/(100.0_r_def)**2)
                          end if
                          theta_pert = dt
                    end select

                    theta(map_wtheta(df) + k) = nl*theta_ref + theta_pert
                end do
            end do
        end if

    end subroutine initial_theta_wtheta_code

end module initial_theta_wtheta_kernel_mod
