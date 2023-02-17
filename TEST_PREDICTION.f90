
!--------------------------------------------------------------------------
!         THIS SUBROUTINE IS WRITTEN TO PREDICT TEST OUTPUTS
!--------------------------------------------------------------------------
  MODULE TEST_PREDICTION
  USE DATA_RESCALING
  
   IMPLICIT NONE
   CONTAINS
   SUBROUTINE sTEST(giHL, giTESTPATS, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, &
   TestingInputs, arTestPREDICTED, arWIFHL_Best, arBIFHL_Best, arWISHL_Best,     &
   arBISHL_Best, arWITHL_Best, arBITHL_Best, arWIOL_Best, arBIOL_Best, grMaxOut, &
   grMinOut, TestingOutputs, TEST_TARGET, TEST_PREDICTED)
   
   INTEGER                    :: I, J, K
   INTEGER, INTENT(IN)        :: giHL, giINPUTS, giFHIDDEN
   INTEGER, INTENT(IN)        :: giSHIDDEN, giTHIDDEN, giTESTPATS
   REAL                       :: grMaxOut, grMinOut 
   REAL                       :: TestingInputs(giTESTPATS, giINPUTS)
   REAL                       :: arWIFHL_Best(giFHIDDEN, giINPUTS) 
   REAL                       :: arWISHL_Best(giSHIDDEN, giFHIDDEN)
   REAL                       :: arWITHL_Best(giTHIDDEN, giSHIDDEN)
   REAL                       :: giONEURONS_N, giONEURONS_A, arBIOL_Best
   REAL, DIMENSION(giFHIDDEN) :: giFNEURONS_N, giFNEURONS_A, arBIFHL_Best
   REAL, DIMENSION(giSHIDDEN) :: giSNEURONS_N, giSNEURONS_A, arBISHL_Best
   REAL, DIMENSION(giTHIDDEN) :: giTNEURONS_N, giTNEURONS_A, arBITHL_Best
   REAL, ALLOCATABLE          :: arWIOL_Best(:), arTestPREDICTED(:), TestingOutputs(:)
   REAL, ALLOCATABLE          :: TEST_TARGET(:), TEST_PREDICTED(:)
   
   PRINT *, ' Test data prediction ...'
   
   ALLOCATE(arTestPREDICTED(giTESTPATS))
  IF(giHL == 1) THEN
  DO K=1,giTESTPATS
    !FIRST HIDDEN LAYER
    DO I=1,giFHIDDEN
       giFNEURONS_N(I)= 0
       DO J=1,giINPUTS
          giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL_Best(I,J)*TestingInputs(K,J) 
       ENDDO
       giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL_Best(I)
       giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
    ENDDO
            
    !OUTPUT LAYER
    giONEURONS_N = 0
    DO I=1,giFHIDDEN 
       giONEURONS_N = giONEURONS_N+arWIOL_Best(I)*giFNEURONS_A(I)
    ENDDO
    giONEURONS_N = giONEURONS_N+arBIOL_Best
    giONEURONS_A = 1/(1+exp(-giONEURONS_N))
    arTestPREDICTED(K)= giONEURONS_A
  ENDDO
  
  ELSEIF(giHL == 2) THEN
  DO K=1,giTESTPATS
     ! FIRST HIDDEN LAYER
     DO I=1,giFHIDDEN
        giFNEURONS_N(I)= 0
        DO J=1,giINPUTS
           giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL_Best(I,J)*TestingInputs(K,J) 
        ENDDO
        giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL_Best(I)
        giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
     ENDDO
            
     !SECOND HIDDEN LAYER
     DO I=1,giSHIDDEN
        giSNEURONS_N(I)= 0
        DO J=1,giFHIDDEN
           giSNEURONS_N(I)= giSNEURONS_N(I)+ arWISHL_Best(I,J)*giFNEURONS_A(J)
        ENDDO
        giSNEURONS_N(I)= giSNEURONS_N(I)+ arBISHL_Best(I)
        giSNEURONS_A(I)= 1/(1+exp(-giSNEURONS_N(I)))
     ENDDO
           
     !OUTPUT LAYER
     giONEURONS_N = 0
     DO I=1,giSHIDDEN 
        giONEURONS_N = giONEURONS_N+arWIOL_Best(I)*giSNEURONS_A(I)
     ENDDO
     giONEURONS_N = giONEURONS_N+arBIOL_Best
     giONEURONS_A = 1/(1+exp(-giONEURONS_N))
     arTestPREDICTED(K)= giONEURONS_A
  ENDDO
   
  ELSE
  DO K=1,giTESTPATS
    ! FIRST HIDDEN LAYER
    DO I=1,giFHIDDEN
       giFNEURONS_N(I)= 0
       DO J=1,giINPUTS
          giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL_Best(I,J)*TestingInputs(K,J) 
       ENDDO
       giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL_Best(I)
       giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
   ENDDO
            
   !SECOND HIDDEN LAYER
   DO I=1,giSHIDDEN
      giSNEURONS_N(I)= 0
      DO J=1,giFHIDDEN
         giSNEURONS_N(I)= giSNEURONS_N(I)+ arWISHL_Best(I,J)*giFNEURONS_A(J)
      ENDDO
      giSNEURONS_N(I)= giSNEURONS_N(I)+ arBISHL_Best(I)
      giSNEURONS_A(I)= 1/(1+exp(-giSNEURONS_N(I)))
   ENDDO
            
   !THIRD HIDDEN LAYER
   DO I=1,giTHIDDEN
      giTNEURONS_N(I)= 0
      DO J=1,giSHIDDEN
         giTNEURONS_N(I)= giTNEURONS_N(I)+ arWITHL_Best(I,J)*giSNEURONS_A(J)
      ENDDO
      giTNEURONS_N(I)= giTNEURONS_N(I)+ arBITHL_Best(I)
      giTNEURONS_A(I)= 1/(1+exp(-giTNEURONS_N(I)))
   ENDDO
           
   !OUTPUT LAYER
   giONEURONS_N = 0
   DO I=1,giTHIDDEN 
      giONEURONS_N = giONEURONS_N+arWIOL_Best(I)*giTNEURONS_A(I)
   ENDDO
   giONEURONS_N = giONEURONS_N+arBIOL_Best
   giONEURONS_A = 1/(1+exp(-giONEURONS_N))
   arTestPREDICTED(K)= giONEURONS_A
 ENDDO 
 ENDIF
 
 
 CALL sRescale(giTESTPATS, TestingOutputs, TEST_TARGET, &
      arTestPREDICTED, TEST_PREDICTED, grMinOut, grMaxOut)
   
  PRINT *, ' Test data predicted OK!'
  PRINT *, ' '
  END SUBROUTINE sTEST
 END MODULE TEST_PREDICTION
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!