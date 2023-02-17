!--------------------------------------------------------------------------
!    THIS SUBROUTINE IS WRITTEN TO CALCULATE SSE FOR VALIDATION DATA
!--------------------------------------------------------------------------
  MODULE SSE_VALIDATION
   IMPLICIT NONE
   CONTAINS
   SUBROUTINE sSSE_VAL(giHL, giVALIDPATS, giINPUTS, giFHIDDEN, giSHIDDEN, &
   giTHIDDEN, ValidationInputs, ValidationOutputs, arWIFHL, arBIFHL,      &
   arWISHL, arBISHL, arWITHL, arBITHL, arWIOL, arBIOL, rSSE_VAL)
   
   INTEGER                    :: I, J, K
   INTEGER, INTENT(IN)        :: giHL, giINPUTS, giFHIDDEN
   INTEGER, INTENT(IN)        :: giSHIDDEN, giTHIDDEN, giVALIDPATS
   REAL                       :: rSSE_VAL, giONEURONS_N, giONEURONS_A, arBIOL 
   REAL                       :: ValidationInputs(giVALIDPATS, giINPUTS)
   REAL                       :: ValidationOutputs(giVALIDPATS)
   REAL                       :: arWIFHL(giFHIDDEN, giINPUTS)
   REAL                       :: arWISHL(giSHIDDEN, giFHIDDEN) 
   REAL                       :: arWITHL(giTHIDDEN, giSHIDDEN)
   REAL, DIMENSION(giFHIDDEN) :: giFNEURONS_N, giFNEURONS_A, arBIFHL
   REAL, DIMENSION(giSHIDDEN) :: giSNEURONS_N, giSNEURONS_A, arBISHL
   REAL, DIMENSION(giTHIDDEN) :: giTNEURONS_N, giTNEURONS_A, arBITHL

   REAL, ALLOCATABLE :: arWIOL(:)
   
   PRINT *, ' Validation data SSE calculation ...'
   
  IF(giHL == 1) THEN
  rSSE_VAL = 0
  DO K=1,giVALIDPATS
    !FIRST HIDDEN LAYER
    DO I=1,giFHIDDEN
       giFNEURONS_N(I)= 0
       DO J=1,giINPUTS
          giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL(I,J)&
          *ValidationInputs(K,J) 
       ENDDO
       giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL(I)
       giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
    ENDDO
            
    !OUTPUT LAYER
    giONEURONS_N = 0
    DO I=1,giFHIDDEN 
       giONEURONS_N = giONEURONS_N+arWIOL(I)*giFNEURONS_A(I)
    ENDDO
    giONEURONS_N = giONEURONS_N+arBIOL
    giONEURONS_A = 1/(1+exp(-giONEURONS_N))
    rSSE_VAL = rSSE_VAL + (ValidationOutputs(K)-giONEURONS_A)**2
  ENDDO
  
  ELSEIF(giHL == 2) THEN
  rSSE_VAL = 0
  DO K=1,giVALIDPATS
     ! FIRST HIDDEN LAYER
     DO I=1,giFHIDDEN
        giFNEURONS_N(I)= 0
        DO J=1,giINPUTS
           giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL(I,J)&
           *ValidationInputs(K,J) 
        ENDDO
        giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL(I)
        giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
     ENDDO
            
     !SECOND HIDDEN LAYER
     DO I=1,giSHIDDEN
        giSNEURONS_N(I)= 0
        DO J=1,giFHIDDEN
           giSNEURONS_N(I)= giSNEURONS_N(I)+ arWISHL(I,J)&
           *giFNEURONS_A(J)
        ENDDO
        giSNEURONS_N(I)= giSNEURONS_N(I)+ arBISHL(I)
        giSNEURONS_A(I)= 1/(1+exp(-giSNEURONS_N(I)))
     ENDDO
           
     !OUTPUT LAYER
     giONEURONS_N = 0
     DO I=1,giSHIDDEN 
        giONEURONS_N = giONEURONS_N+arWIOL(I)&
        *giSNEURONS_A(I)
     ENDDO
     giONEURONS_N = giONEURONS_N+arBIOL
     giONEURONS_A = 1/(1+exp(-giONEURONS_N))
     rSSE_VAL = rSSE_VAL + (ValidationOutputs(K)-giONEURONS_A)**2
  ENDDO
   
  ELSE
  rSSE_VAL = 0
  DO K=1,giVALIDPATS
    ! FIRST HIDDEN LAYER
    DO I=1,giFHIDDEN
       giFNEURONS_N(I)= 0
       DO J=1,giINPUTS
          giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL(I,J)&
          *ValidationInputs(K,J) 
       ENDDO
       giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL(I)
       giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
   ENDDO
            
   !SECOND HIDDEN LAYER
   DO I=1,giSHIDDEN
      giSNEURONS_N(I)= 0
      DO J=1,giFHIDDEN
         giSNEURONS_N(I)= giSNEURONS_N(I)+ arWISHL(I,J)&
         *giFNEURONS_A(J)
      ENDDO
      giSNEURONS_N(I)= giSNEURONS_N(I)+ arBISHL(I)
      giSNEURONS_A(I)= 1/(1+exp(-giSNEURONS_N(I)))
   ENDDO
            
   !THIRD HIDDEN LAYER
   DO I=1,giTHIDDEN
      giTNEURONS_N(I)= 0
      DO J=1,giSHIDDEN
         giTNEURONS_N(I)= giTNEURONS_N(I)+ arWITHL(I,J)&
         *giSNEURONS_A(J)
      ENDDO
      giTNEURONS_N(I)= giTNEURONS_N(I)+ arBITHL(I)
      giTNEURONS_A(I)= 1/(1+exp(-giTNEURONS_N(I)))
   ENDDO
           
   !OUTPUT LAYER
   giONEURONS_N = 0
   DO I=1,giTHIDDEN 
      giONEURONS_N = giONEURONS_N+arWIOL(I)*giTNEURONS_A(I)
   ENDDO
   giONEURONS_N = giONEURONS_N+arBIOL
   giONEURONS_A = 1/(1+exp(-giONEURONS_N))
   rSSE_VAL = rSSE_VAL + (ValidationOutputs(K)-giONEURONS_A)**2
 ENDDO 
 ENDIF
   
  PRINT *, ' Validation data SSE calculated OK!'
  PRINT *, ' '
  END SUBROUTINE sSSE_VAL
 END MODULE SSE_VALIDATION
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!