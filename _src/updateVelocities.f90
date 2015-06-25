SUBROUTINE updateVelocities(u,v,uEdge,vEdge,time)
  ! =========================================================
  ! Updates the edge and quadrature velocities to given time
  ! =========================================================
  USE commonTestParameters
  IMPLICIT NONE
  ! Inputs
  DOUBLE PRECISION, INTENT(IN) :: time
  ! Outputs
  DOUBLE PRECISION, DIMENSION(1:nxOut,1:nyOut), INTENT(INOUT) :: u,v
  DOUBLE PRECISION, DIMENSION(1:nex,1:nyOut), INTENT(INOUT) :: uEdge
  DOUBLE PRECISION, DIMENSION(1:nxOut,1:ney), INTENT(INOUT) :: vEdge

  ! Local Variables
  DOUBLE PRECISION :: timeFactor

  INTERFACE
    function tfcn(t)
      USE commonTestParameters
      ! Inputs
      DOUBLE PRECISION, intent(in) :: t
      ! Outputs
      DOUBLE PRECISION :: tfcn
    END FUNCTION tfcn
  END INTERFACE

  ! Update time-dependent flows
  IF(transient) THEN
    timeFactor = tfcn(time)
    u = u*timeFactor
    v = v*timeFactor
    uEdge = uEdge*timeFactor
    vEdge = vEdge*timeFactor
  ENDIF

  ! Add background mean flow
  u = u + uMean
  v = v + vMean

END SUBROUTINE updateVelocities
