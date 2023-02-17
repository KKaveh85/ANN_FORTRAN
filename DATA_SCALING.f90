
!--------------------------------------------------------------------------
!      THIS SUBROUTINE IS WRITTEN TO SCALE THE DATA BETWEEN 0 AND 1
!--------------------------------------------------------------------------
 MODULE DATA_SCALING
  IMPLICIT NONE
  CONTAINS
  
  SUBROUTINE sScale(giPATS, giINPUTS, In, garInput, Tar, garOutputs, &
  grMinOut, grMaxOut)
  INTEGER                          :: I
  INTEGER, INTENT(IN)              :: giPATS, giINPUTS
  REAL, ALLOCATABLE                :: garMaxInp(:), garMinInp(:)
  REAL, ALLOCATABLE, INTENT(INOUT) :: In(:,:), garInput(:,:)
  REAL, ALLOCATABLE, INTENT(INOUT) :: garOutputs(:), Tar(:)
  REAL, INTENT(INOUT)              :: grMinOut, grMaxOut   

  PRINT *, ' Normalising data...'
   
  ALLOCATE(garMaxInp(giINPUTS))
  ALLOCATE(garMinInp(giINPUTS))
  ALLOCATE(garInput(giPATS,giINPUTS))
  ALLOCATE(garOutputs(giPATS))
   
  garMaxInp(:) = MAXVAL(In,1)
  garMinInp(:) = MINVAL(In,1)
  grMaxOut = MAXVAL(Tar(:))
  grMinOut = MINVAL(Tar(:))
	
  ! scale between 0 and 1
  DO I=1,giPATS
     garInput(I,1:giINPUTS) = &
     (In(I,1:giINPUTS) - garMinInp(1:giINPUTS)) / &
     (garMaxInp(1:giINPUTS) - garMinInp(1:giINPUTS))
  ENDDO
	
  garOutputs(:) = &
  (Tar(:) - grMinOut) / (grMaxOut - grMinOut)
	
  PRINT *, ' Data normalised OK!'
  PRINT *, ' '
 END SUBROUTINE sScale
END MODULE DATA_SCALING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!