!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
module generate_global_gw_fields_mod
!> @brief module that contains routines taken to initialise fields based upon
!> DCMIP test 31 - non-hydrostatic gravity waves
!> @detail The non-hydrostatic gravity wave test examines the response of models to short time-scale wavemotion
!> triggered by a localized perturbation. The formulation presented in this document is new,
!> but is based on previous approaches by Skamarock et al. (JAS 1994), Tomita and Satoh (FDR 2004), and
!> Jablonowski et al. (NCAR Tech Report 2008) 

use constants_mod, only: r_def, pi, gravity, cp, p_zero, &
                         omega, earth_radius, n_sq, kappa, rd

implicit none

contains

subroutine generate_global_gw_fields (lat, z, exner, u, theta, rho)

implicit none
        
  real(kind=r_def), intent(in)  :: lat, z ! Latitude (radians) and Height (m)
                                   
  real(kind=r_def), intent(out) :: u(3), &               ! (Zonal,Meridional,Vertical) wind (m s^-1)
                                   theta, &              ! potential Temperature (K)
                                   exner, &              ! exner pressure
                                   rho                   ! density (kg m^-3)

  real(kind=r_def), parameter :: u0        = 0.0_r_def,    &     ! Reference Velocity 
                                 t_equator = 300.0_r_def,   &     ! Temperature at Equator    
                                 ztop      = 10000.0_r_def        ! Model Top       
                           
  real(kind=r_def) :: bigG = (gravity*gravity)/(n_sq*cp)      ! G constant from DCMIP formulation                            
  real(kind=r_def) :: tsurf, psurf                            ! Surface temperature (k) and pressure (Pa)
  real(kind=r_def) :: temperature, pressure                   ! temperature(k) and pressure (Pa)
  real(kind=r_def) :: exp_fac
  real(kind=r_def) :: p_equator = p_zero

! intialise wind field
  u(1) = u0 * cos(lat)
  u(2) = 0.0_r_def
  u(3) = 0.0_r_def

! 
  exp_fac = (u0+2.0_r_def*omega*earth_radius)*(cos(2.0_r_def*lat)-1.0_r_def)

! Compute surface temperture
  tsurf = bigG + (t_equator - bigG)*exp( -(u0*n_sq/(4.0_r_def*gravity*gravity))*exp_fac ) 

! Compute surface pressure
  psurf = p_equator*exp( (u0/(4.0_r_def*bigG*rd))*exp_fac  ) * (tsurf/t_equator)**(cp/rd)

! Compute pressure and temperature
  pressure = psurf*( (bigG/tsurf)*exp(-n_sq*z/gravity)+1.0_r_def - (bigG/tsurf)  )**(cp/rd)

  temperature = bigG*(1.0_r_def - exp(n_sq*z/gravity))+ tsurf*exp(n_sq*z/gravity)

! Compute density from equation of state
  rho = pressure/(rd*temperature)

! convert pressure to exner pressure and temperature to potential temperature
  exner = (pressure/p_zero)**kappa
  theta = temperature/exner

end subroutine generate_global_gw_fields

!=================================================================================

pure function generate_global_gw_pert(lon, lat, z) result(theta)
!> @brief Function to generate the potential temperature pertubation for 
!> the global gravity wave test
implicit none

  real(kind=r_def)              :: theta
  real(kind=r_def), intent(in)  :: lon, lat, z

  real(kind=r_def) :: sin_tmp, cos_tmp, r, shape_function

  real(kind=r_def), parameter :: lambdac = 2.0_r_def*pi/3.0_r_def,     &     ! Lon of Pert Center
                                 d       = 5000.0_r_def,               &     ! Width for Pert
                                 phic    = 0.0_r_def,                  &     ! Lat of Pert Center
                                 delta_theta = 1.0_r_def,              &     ! Max Amplitude of Pert
                                 Lz      = 10000.0_r_def                     ! Vertical half-Wavelength of Pert
 
  sin_tmp = sin(lat) * sin(phic)
  cos_tmp = cos(lat) * cos(phic)

! great circle distance  
  r  = earth_radius * acos (sin_tmp + cos_tmp*cos(lon-lambdac)) 

  shape_function = (d**2)/(d**2 + r**2)

  theta = delta_theta*shape_function*sin(pi*z/Lz)
end function generate_global_gw_pert

end module generate_global_gw_fields_mod

