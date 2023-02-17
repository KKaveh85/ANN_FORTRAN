
!--------------------------------------------------------------------------
!     THIS SUBROUTINE IS WRITTEN TO RESCALE THE DATA BETWEEN 0 AND 1
!--------------------------------------------------------------------------
 MODULE DATA_RESCALING
  IMPLICIT NONE
  CONTAINS
  
  SUBROUTINE sRescale(giPATS, ScaledTarget, RescaledTarget, &
  ScaledPredicted, RescaledPredicted, grMinOut, grMaxOut)
  
  INTEGER, INTENT(IN)              :: giPATS
  REAL, ALLOCATABLE, INTENT(INOUT) :: ScaledTarget(:), RescaledTarget(:)
  REAL, ALLOCATABLE, INTENT(INOUT) :: ScaledPredicted(:), RescaledPredicted(:)
  REAL                             :: grMinOut, grMaxOut   

  PRINT *, ' Rescaling data...'

  ALLOCATE(RescaledTarget(giPATS))
  ALLOCATE(RescaledPredicted(giPATS))
   
	
  ! rescaling data
  RescaledTarget(:) = ScaledTarget(:)*&
  (grMaxOut - grMinOut)+grMinOut
	
  RescaledPredicted(:) = ScaledPredicted(:)*&
  (grMaxOut - grMinOut)+grMinOut
	
  PRINT *, ' Data rescaled OK!'
  PRINT *, ' '
 END SUBROUTINE sRescale
END MODULE DATA_RESCALING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!