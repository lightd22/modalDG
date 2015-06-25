SUBROUTINE reactiveForcing(forcing,qVals,forcingCoeffs)
  ! ==============================================================================
  ! Computes right hand side forcing terms for chemical reaction equation
  ! INPUTS:
  !         qVals(1:meqn) - solution values at given points
  !         forcingCoeffs(1:meqn) - forcing coefficients multiplying fields q1,..qmeqn
  ! OUTPUTS: forcing(1:meqn) - RHS forcing function for fields q1,...,qmeqn
  !
  ! ==============================================================================
  USE commonTestParameters
  IMPLICIT NONE
  ! Inputs
  DOUBLE PRECISION, DIMENSION(1:meqn), INTENT(IN) :: qVals,forcingCoeffs
  ! Outputs
  DOUBLE PRECISION, DIMENSION(1:meqn), INTENT(OUT) :: forcing
  ! Local variables
  INTEGER :: j

  if(meqn > 3) then
    write(*,*) 'in reactiveForcing: warning! not set up for more than three reacting tracers'
    STOP
  endif
  forcing = 0D0
  ! Evaluate reactive forcing function at grid points

  ! ====
  ! dq1/dt = -r1*q1+r2*q2^2
  ! dq2/dt = -2*dq1/dt
  ! ====
!  forcing(1) = -forcingCoeffs(1)*qVals(1)+forcingCoeffs(2)*qVals(2)**2
!  forcing(2) = -2D0*forcing(1)

  ! ====
  ! dq1/dt = -r*q1*q2
  ! dq2/dt = -dq1/dt
  ! ====
!  forcing(1) = -forcingCoeffs(1)*qVals(1)*qvals(2)
!  forcing(2) = -1D0*forcing(1)

  ! ====
  ! dq1/dt = r*q2^2
  ! dq2/dt = -dq1/dt
  ! ====
  forcing(1) = forcingCoeffs(1)*qVals(2)*qVals(2)
  forcing(2) = -1D0*forcing(1)

!  forcing(1) = -forcingCoeffs(1)*qVals(1)*qvals(2)
!  forcing(2) = forcing(1)
!  forcing(3) = -2D0*forcing(1)

END SUBROUTINE reactiveForcing
