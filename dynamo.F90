  program dynamo
    use lfric
    use psy
    use gaussian_quadrature_mod
    implicit none

    type(functionSpace) :: v3FunctionSpace
    type(field) :: pressureDensity

    call init_gauss()

    write(*,*) 'hello, world'

    v3FunctionSpace = functionSpace(5,1)
    write(*,'("Dynamo:Created v3 function space: need to read mesh and connectivity data")') 
    pressureDensity = field(v3FunctionSpace,5)

    

    call invoke_RHS_V3(v3FunctionSpace)
    ! call invoke(RHS_V3(arg) )
    call test_integrate()
    call final_gauss()

  end program dynamo
