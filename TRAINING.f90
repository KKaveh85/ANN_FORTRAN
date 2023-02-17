
!--------------------------------------------------------------------------
!            THIS SUBROUTINE IS WRITTEN TO KEEP BEST WEIGHTS
!--------------------------------------------------------------------------
 MODULE TRAINING
   USE JACOBIAN_CalALCULATION
   USE HESSIAN_INVERSE
   USE WEIGHTS_ADJUSTING
   USE SSE_CALCULATION
   USE SSE_VALIDATION
   USE KEEP_BEST_WEIGHTS
   USE EARLY_STOPPING
   
   IMPLICIT NONE
   CONTAINS
   SUBROUTINE sTrain(giINPUTS, giHL, giTRAINPATS, giVALIDPATS, giLENGTH, giFHIDDEN,   &
   giSHIDDEN, giTHIDDEN, iEpch, iEpchMAX, arWIFHL, arBIFHL, arWISHL, arBISHL, arWITHL,&
   arBITHL, arWIOL, arBIOL, giERROR, JACOBIAN, iSTOP, JACOBIAN_TRANSPOSE, HESSIAN,    &
   giHESSIAN_INVERSE, TrainingInputs, TrainingOutputs, arWIFHL_NEW, ValidationInputs, &
   ValidationOutputs, arBIFHL_NEW, arWISHL_NEW, arBISHL_NEW, arWITHL_NEW, arBITHL_NEW,&
   arWIOL_NEW, arBIOL_NEW, arWIFHL_Best, arBIFHL_Best, arWISHL_Best, arBISHL_Best,    &
   arWITHL_Best, arBITHL_Best, arWIOL_Best, arBIOL_Best,rSSE_OLD, rSSE_NEW, rSSE_VAL, &
   rSSE_VALBest, VAL_ERROR, iFINDER, giCC)

   INTEGER              :: iEpch, N, iSTOP
   INTEGER, INTENT(IN)  :: giINPUTS, giHL, giTRAINPATS, giLENGTH, giVALIDPATS
   INTEGER, INTENT(IN)  :: giFHIDDEN, giSHIDDEN, giTHIDDEN, iEpchMAX, iFINDER
   REAL                 :: rSSE_OLD, rSSE_NEW, arBIOL, arBIOL_NEW, arBIOL_Best
   REAL                 :: rSSE_VALBest, giCC, rSSE_VAL 
   REAL                 :: TrainingOutputs(giTRAINPATS)
   REAL                 :: ValidationOutputs(giVALIDPATS)
   REAL                 :: TrainingInputs(giTRAINPATS, giINPUTS)
   REAL                 :: ValidationInputs(giVALIDPATS, giINPUTS) 
   REAL, ALLOCATABLE    :: arBIFHL(:), arBIFHL_NEW(:), arBIFHL_Best(:)
   REAL, ALLOCATABLE    :: arBISHL(:), arBISHL_NEW(:), arBISHL_Best(:)
   REAL, ALLOCATABLE    :: arBITHL(:), arBITHL_NEW(:), arBITHL_Best(:)
   REAL, ALLOCATABLE    :: arWIFHL_NEW(:,:), arWIFHL_Best(:,:), arWIFHL(:,:)
   REAL, ALLOCATABLE    :: arWISHL_NEW(:,:), arWISHL_Best(:,:), arWISHL(:,:)
   REAL, ALLOCATABLE    :: arWITHL_NEW(:,:), arWITHL_Best(:,:), arWITHL(:,:)
   REAL, ALLOCATABLE    :: giHESSIAN_INVERSE(:,:), giERROR(:), VAL_ERROR(:)
   REAL, ALLOCATABLE    :: HESSIAN(:,:), JACOBIAN_TRANSPOSE(:,:), JACOBIAN(:,:)
   REAL, ALLOCATABLE    :: arWIOL(:), arWIOL_NEW(:), arWIOL_Best(:)
   
   ALLOCATE(VAL_ERROR(iEpchMAX))
   
   iEpch = 1
   VAL_ERROR = 0 
   DO WHILE(iEpch <= iEpchMAX) 
   
   111  CALL sJacobian(giINPUTS, giHL, giTRAINPATS, giLENGTH, giFHIDDEN,   &
        giSHIDDEN, giTHIDDEN, arWIFHL, arBIFHL, arWISHL, arBISHL, arWITHL, &
        arBITHL, arWIOL, arBIOL, giERROR, JACOBIAN, TrainingInputs, TrainingOutputs)
 
   CALL sHessian(giLENGTH, giTRAINPATS, JACOBIAN, JACOBIAN_TRANSPOSE, giCC, &
   HESSIAN, giHESSIAN_INVERSE)
 
   CALL sWeight(giINPUTS, giLENGTH, giTRAINPATS, giFHIDDEN, giSHIDDEN,      &
   giTHIDDEN, giHL, giHESSIAN_INVERSE, JACOBIAN_TRANSPOSE, giERROR, arWIFHL,&
   arBIFHL, arWISHL, arBISHL, arWITHL, arBITHL, arWIOL, arBIOL, arWIFHL_NEW,&
   arBIFHL_NEW, arWISHL_NEW, arBISHL_NEW, arWITHL_NEW, arBITHL_NEW, arWIOL_NEW, arBIOL_NEW)
  
   CALL sSSE(giHL, giTRAINPATS, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN,  &
   giERROR, TrainingInputs, TrainingOutputs, arWIFHL_NEW, arBIFHL_NEW,      &
   arWISHL_NEW, arBISHL_NEW, arWITHL_NEW, arBITHL_NEW,arWIOL_NEW, arBIOL_NEW, rSSE_OLD, rSSE_NEW)
   
   IF(rSSE_NEW <= rSSE_OLD) THEN 
      giCC = giCC/10
      arWIFHL = arWIFHL_NEW
      arBIFHL = arBIFHL_NEW
      arWISHL = arWISHL_NEW
      arBISHL = arBISHL_NEW
      arWITHL = arWITHL_NEW
      arBITHL = arBITHL_NEW
      arWIOL = arWIOL_NEW
      arBIOL = arBIOL_NEW
      GOTO 999
   ELSEIF (rSSE_NEW > rSSE_OLD .AND. giCC <= 1e10) THEN
      giCC = giCC*10
      DEALLOCATE(giERROR)
      DEALLOCATE(JACOBIAN)
      DEALLOCATE(HESSIAN)
      DEALLOCATE(JACOBIAN_TRANSPOSE)
      DEALLOCATE(giHESSIAN_INVERSE)
      DEALLOCATE(arWIFHL_NEW)
      DEALLOCATE(arBIFHL_NEW)
      DEALLOCATE(arWISHL_NEW)
      DEALLOCATE(arBISHL_NEW)
      DEALLOCATE(arWITHL_NEW)
      DEALLOCATE(arBITHL_NEW)
      DEALLOCATE(arWIOL_NEW)  
      GOTO 111
   ELSEIF (rSSE_NEW > rSSE_OLD .AND. giCC > 1e10) THEN
      GOTO 999    
   ENDIF
  
  
   
  999  DEALLOCATE(giERROR)
       DEALLOCATE(JACOBIAN)
       DEALLOCATE(HESSIAN)
       DEALLOCATE(JACOBIAN_TRANSPOSE)
       DEALLOCATE(giHESSIAN_INVERSE)
       DEALLOCATE(arWIFHL_NEW)
       DEALLOCATE(arBIFHL_NEW)
       DEALLOCATE(arWISHL_NEW)
       DEALLOCATE(arBISHL_NEW)
       DEALLOCATE(arWITHL_NEW)
       DEALLOCATE(arBITHL_NEW)
       DEALLOCATE(arWIOL_NEW)
       
   
   CALL sSSE_VAL(giHL, giVALIDPATS, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, &
   ValidationInputs, ValidationOutputs, arWIFHL, arBIFHL, arWISHL, arBISHL,    &
   arWITHL, arBITHL, arWIOL, arBIOL, rSSE_VAL) 
   
   VAL_ERROR(iEpch) = rSSE_VAL
   
   OPEN (unit=11,file="C:\Users\mesha\OneDrive\Desktop\results.txt",action="write")
   WRITE (11,fmt='(4(X, F14.8))') iEpch, rSSE_NEW, rSSE_OLD, rSSE_VAL
   
   IF(iEpch .EQ. 1) THEN
     CALL sKEEP(iEpch, giHL, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, rSSE_VAL,&
     rSSE_VALBest, arBIFHL, arBIFHL_Best, arBISHL, arBISHL_Best, arBITHL,        &
     arBITHL_Best, arWIFHL_Best, arWIFHL, arWISHL_Best, arWISHL, arWITHL_Best,   &
     arWITHL, arWIOL, arWIOL_Best, arBIOL, arBIOL_Best)
     
   ELSEIF(iEpch >= 2 .AND. rSSE_VAL <= rSSE_VALBest) THEN
     DEALLOCATE(arWIFHL_Best)
     DEALLOCATE(arWISHL_Best)
     DEALLOCATE(arWITHL_Best)
     DEALLOCATE(arBIFHL_Best)
     DEALLOCATE(arBISHL_Best)
     DEALLOCATE(arBITHL_Best)
     DEALLOCATE(arWIOL_Best)
     CALL sKEEP(iEpch, giHL, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, rSSE_VAL, &
     rSSE_VALBest, arBIFHL, arBIFHL_Best, arBISHL, arBISHL_Best, arBITHL,         &
     arBITHL_Best, arWIFHL_Best, arWIFHL, arWISHL_Best, arWISHL, arWITHL_Best,    &
     arWITHL, arWIOL, arWIOL_Best, arBIOL, arBIOL_Best)
     N = iEpch
   ENDIF
   
   
   IF(iEpch >= iSTOP+1) THEN
      CALL sEARLY_STOPPING(iEpch, VAL_ERROR, iSTOP, iFINDER)
      IF(iFINDER .EQ. 0) THEN
        GOTO 101
      ENDIF
   ENDIF
   
   iEpch = iEpch + 1
   PRINT*,iEpch
   ENDDO 
   
   101  OPEN (unit = 13, file = "C:\Users\mesha\OneDrive\Desktop\results3.txt", action="write")
        WRITE (13, fmt='((X, F14.8))') arBIFHL_Best 
        PRINT*,N 
END SUBROUTINE sTrain 
END MODULE TRAINING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!