 
!--------------------------------------------------------------------------
!            THIS SUBROUTINE IS WRITTEN TO CALCULATE SSE
!--------------------------------------------------------------------------
 MODULE SSE_CALCULATION
  IMPLICIT NONE
  CONTAINS
  SUBROUTINE sSSE(giHL, giTRAINPATS, giINPUTS, giFHIDDEN, giSHIDDEN, &
  giTHIDDEN, giERROR, TrainingInputs, TrainingOutputs, arWIFHL_NEW,  &
  arBIFHL_NEW, arWISHL_NEW, arBISHL_NEW, arWITHL_NEW, arBITHL_NEW,   &
  arWIOL_NEW, arBIOL_NEW, rSSE_OLD, rSSE_NEW)
   
  INTEGER                    :: I, J, K
  INTEGER, INTENT(IN)        :: giHL, giTRAINPATS, giINPUTS, giFHIDDEN
  INTEGER, INTENT(IN)        :: giSHIDDEN, giTHIDDEN
  REAL                       :: rSSE_OLD, rSSE_NEW, giONEURONS_N
  REAL                       :: giONEURONS_A, arBIOL_NEW
  REAL                       :: TrainingOutputs(giTRAINPATS)
  REAL                       :: TrainingInputs(giTRAINPATS, giINPUTS)
  REAL                       :: arWIFHL_NEW(giFHIDDEN, giINPUTS)
  REAL                       :: arWISHL_NEW(giSHIDDEN, giFHIDDEN)
  REAL                       :: arWITHL_NEW(giTHIDDEN, giSHIDDEN)
  REAL, DIMENSION(giFHIDDEN) :: giFNEURONS_N, giFNEURONS_A, arBIFHL_NEW
  REAL, DIMENSION(giSHIDDEN) :: giSNEURONS_N, giSNEURONS_A, arBISHL_NEW
  REAL, DIMENSION(giTHIDDEN) :: giTNEURONS_N, giTNEURONS_A, arBITHL_NEW
  REAL, ALLOCATABLE          :: arWIOL_NEW(:), giERROR(:)
   
  PRINT *, ' SSE calculation ...'
  rSSE_OLD = 0
  DO I=1,giTRAINPATS
     rSSE_OLD = rSSE_OLD + giERROR(I)**2
  ENDDO
   
 IF(giHL == 1) THEN
 rSSE_NEW = 0
 DO K=1,giTRAINPATS
   !FIRST HIDDEN LAYER
   DO I=1,giFHIDDEN
      giFNEURONS_N(I)= 0
      DO J=1,giINPUTS
         giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL_NEW(I,J)&
         *TrainingInputs(K,J) 
      ENDDO
      giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL_NEW(I)
      giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
   ENDDO
            
   !OUTPUT LAYER
   giONEURONS_N = 0
   DO I=1,giFHIDDEN 
      giONEURONS_N = giONEURONS_N+arWIOL_NEW(I)*giFNEURONS_A(I)
   ENDDO
   giONEURONS_N = giONEURONS_N+arBIOL_NEW
   giONEURONS_A = 1/(1+exp(-giONEURONS_N))
   rSSE_NEW = rSSE_NEW + (TrainingOutputs(K)-giONEURONS_A)**2
 ENDDO
  
 ELSEIF(giHL == 2) THEN
 rSSE_NEW = 0
 DO K=1,giTRAINPATS
    ! FIRST HIDDEN LAYER
    DO I=1,giFHIDDEN
       giFNEURONS_N(I)= 0
       DO J=1,giINPUTS
          giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL_NEW(I,J)&
          *TrainingInputs(K,J) 
       ENDDO
       giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL_NEW(I)
       giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
    ENDDO
            
    !SECOND HIDDEN LAYER
    DO I=1,giSHIDDEN
       giSNEURONS_N(I)= 0
       DO J=1,giFHIDDEN
          giSNEURONS_N(I)= giSNEURONS_N(I)+ arWISHL_NEW(I,J)&
          *giFNEURONS_A(J)
       ENDDO
       giSNEURONS_N(I)= giSNEURONS_N(I)+ arBISHL_NEW(I)
       giSNEURONS_A(I)= 1/(1+exp(-giSNEURONS_N(I)))
    ENDDO
           
    !OUTPUT LAYER
    giONEURONS_N = 0
    DO I=1,giSHIDDEN 
       giONEURONS_N = giONEURONS_N+arWIOL_NEW(I)*giSNEURONS_A(I)
    ENDDO
    giONEURONS_N = giONEURONS_N+arBIOL_NEW
    giONEURONS_A = 1/(1+exp(-giONEURONS_N))
    rSSE_NEW = rSSE_NEW + (TrainingOutputs(K)-giONEURONS_A)**2
 ENDDO
   
 ELSE
 rSSE_NEW = 0
 DO K=1,giTRAINPATS
   ! FIRST HIDDEN LAYER
   DO I=1,giFHIDDEN
      giFNEURONS_N(I)= 0
      DO J=1,giINPUTS
         giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL_NEW(I,J)&
         *TrainingInputs(K,J) 
      ENDDO
      giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL_NEW(I)
      giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
  ENDDO
            
  !SECOND HIDDEN LAYER
  DO I=1,giSHIDDEN
     giSNEURONS_N(I)= 0
     DO J=1,giFHIDDEN
        giSNEURONS_N(I)= giSNEURONS_N(I)+ arWISHL_NEW(I,J)&
        *giFNEURONS_A(J)
     ENDDO
     giSNEURONS_N(I)= giSNEURONS_N(I)+ arBISHL_NEW(I)
     giSNEURONS_A(I)= 1/(1+exp(-giSNEURONS_N(I)))
  ENDDO
            
 !THIRD HIDDEN LAYER
 DO I=1,giTHIDDEN
    giTNEURONS_N(I)= 0
    DO J=1,giSHIDDEN
       giTNEURONS_N(I)= giTNEURONS_N(I)+ arWITHL_NEW(I,J)&
       *giSNEURONS_A(J)
    ENDDO
    giTNEURONS_N(I)= giTNEURONS_N(I)+ arBITHL_NEW(I)
    giTNEURONS_A(I)= 1/(1+exp(-giTNEURONS_N(I)))
 ENDDO
           
 !OUTPUT LAYER
 giONEURONS_N = 0
 DO I=1,giTHIDDEN 
    giONEURONS_N = giONEURONS_N+arWIOL_NEW(I)*giTNEURONS_A(I)
 ENDDO
 giONEURONS_N = giONEURONS_N+arBIOL_NEW
 giONEURONS_A = 1/(1+exp(-giONEURONS_N))
 rSSE_NEW = rSSE_NEW + (TrainingOutputs(K)-giONEURONS_A)**2
 ENDDO 
 ENDIF
   
 PRINT *, ' SSE calculated OK!'
 PRINT *, ' '
 END SUBROUTINE sSSE
END MODULE SSE_CALCULATION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1