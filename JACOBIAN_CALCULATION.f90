
!--------------------------------------------------------------------------
!         THIS SUBROUTINE IS WRITTEN TO CALCULATE JACOBIAN MATRIX
!--------------------------------------------------------------------------
 MODULE JACOBIAN_CalALCULATION
 IMPLICIT NONE
 CONTAINS
 
 SUBROUTINE sJacobian(giINPUTS, giHL, giTRAINPATS, giLENGTH, giFHIDDEN, &
 giSHIDDEN, giTHIDDEN, arWIFHL, arBIFHL, arWISHL, arBISHL, arWITHL, &
 arBITHL, arWIOL, arBIOL, giERROR, JACOBIAN, TrainingInputs, TrainingOutputs)
  
  INTEGER             :: I, J, K, L 
  INTEGER             :: giINPUTS, giHL, giTRAINPATS, giLENGTH 
  INTEGER             :: giFHIDDEN, giSHIDDEN, giTHIDDEN
  REAL                :: arWIFHL(giFHIDDEN,giINPUTS), arBIFHL(giFHIDDEN)
  REAL                :: arWISHL(giSHIDDEN,giFHIDDEN), arBISHL(giSHIDDEN)
  REAL                :: arWITHL(giTHIDDEN,giSHIDDEN), arBITHL(giTHIDDEN)
  REAL                :: TrainingOutputs(giTRAINPATS), TrainingInputs(giTRAINPATS,giINPUTS) 
  REAL                :: giONEURONS_N, giONEURONS_A, giONEURONS_S, giODELTA, arBIOL
  REAL, ALLOCATABLE   :: giFNEURONS_N(:), giFNEURONS_A(:), giFNEURONS_S(:)
  REAL, ALLOCATABLE   :: giSNEURONS_N(:), giSNEURONS_A(:), giSNEURONS_S(:)
  REAL, ALLOCATABLE   :: giTNEURONS_N(:), giTNEURONS_A(:), giTNEURONS_S(:)
  REAL, ALLOCATABLE   :: arWIOL(:), giFDELTA(:), giERROR(:), JACOBIAN(:,:)
  REAL, ALLOCATABLE   :: giSDELTA(:), giTDELTA(:)  
  
  ALLOCATE(giFNEURONS_N(giFHIDDEN))
  ALLOCATE(giFNEURONS_A(giFHIDDEN))
  ALLOCATE(giFNEURONS_S(giFHIDDEN))
  ALLOCATE(giSNEURONS_N(giSHIDDEN))
  ALLOCATE(giSNEURONS_A(giSHIDDEN))
  ALLOCATE(giSNEURONS_S(giSHIDDEN))
  ALLOCATE(giTNEURONS_N(giTHIDDEN))
  ALLOCATE(giTNEURONS_A(giTHIDDEN))
  ALLOCATE(giTNEURONS_S(giTHIDDEN))
  ALLOCATE(giFDELTA(giFHIDDEN))
  ALLOCATE(giSDELTA(giSHIDDEN))
  ALLOCATE(giTDELTA(giTHIDDEN))
  ALLOCATE(giERROR(giTRAINPATS))
  ALLOCATE(JACOBIAN(giTRAINPATS,giLENGTH))
  
  PRINT *, ' Matrix Jacobian calculation...'

IF(giHL==1) THEN
 DO K=1,giTRAINPATS
    !FIRST HIDDEN LAYER
    DO I=1,giFHIDDEN
       giFNEURONS_N(I)= 0
       DO J=1,giINPUTS
          giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL(I,J)*TrainingInputs(K,J) 
       ENDDO
       giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL(I)
       giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
       giFNEURONS_S(I)= (exp(-giFNEURONS_N(I)))/((1+exp(-giFNEURONS_N(I)))**2)
    ENDDO
            
    !OUTPUT LAYER
    giONEURONS_N = 0
    DO I=1,giFHIDDEN 
       giONEURONS_N = giONEURONS_N+arWIOL(I)*giFNEURONS_A(I)
    ENDDO
    giONEURONS_N = giONEURONS_N+arBIOL
    giONEURONS_A = 1/(1+exp(-giONEURONS_N))
    giONEURONS_S = (exp(-giONEURONS_N))/((1+exp(-giONEURONS_N))**2)
    giERROR(K)= TrainingOutputs(K)-giONEURONS_A
      
    giODELTA = giONEURONS_S
    DO I=1,giFHIDDEN
       giFDELTA(I)= giFNEURONS_S(I)*giODELTA*arWIOL(I)
    ENDDO 
             
    !JACOBIAN MATRIX CALCULATION
    DO J=1,giFHIDDEN
       DO I=1,giINPUTS
          JACOBIAN(K,I+(J-1)*giINPUTS)= -1*giFDELTA(J)*TrainingInputs(k,I)
          JACOBIAN(K,J+giFHIDDEN*giINPUTS)= -1*giFDELTA(J)
       ENDDO
    ENDDO

    DO I=1,giFHIDDEN
       JACOBIAN(K,I+giFHIDDEN*(giINPUTS+1))= -1*giODELTA*giFNEURONS_A(I)
    ENDDO
    JACOBIAN(K,giFHIDDEN*(giINPUTS+2)+1)= -1*giODELTA     
 ENDDO
 
 ELSEIF(giHL==2) THEN
  DO K=1,giTRAINPATS
     ! FIRST HIDDEN LAYER
     DO I=1,giFHIDDEN
        giFNEURONS_N(I)= 0
        DO J=1,giINPUTS
           giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL(I,J)*TrainingInputs(K,J) 
        ENDDO
        giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL(I)
        giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
        giFNEURONS_S(I)= (exp(-giFNEURONS_N(I)))/((1+exp(-giFNEURONS_N(I)))**2)
     ENDDO
            
     !SECOND HIDDEN LAYER
     DO I=1,giSHIDDEN
        giSNEURONS_N(I)= 0
        DO J=1,giFHIDDEN
           giSNEURONS_N(I)= giSNEURONS_N(I)+ arWISHL(I,J)*giFNEURONS_A(J)
        ENDDO
        giSNEURONS_N(I)= giSNEURONS_N(I)+ arBISHL(I)
        giSNEURONS_A(I)= 1/(1+exp(-giSNEURONS_N(I)))
        giSNEURONS_S(I)= (exp(-giSNEURONS_N(I)))/((1+exp(-giSNEURONS_N(I)))**2)
     ENDDO
           
     !OUTPUT LAYER
     giONEURONS_N = 0
     DO I=1,giSHIDDEN 
        giONEURONS_N = giONEURONS_N+arWIOL(I)*giSNEURONS_A(I)
     ENDDO
     giONEURONS_N = giONEURONS_N+arBIOL
     giONEURONS_A = 1/(1+exp(-giONEURONS_N))
     giONEURONS_S = (exp(-giONEURONS_N))/((1+exp(-giONEURONS_N))**2)
            
     !ERROR CALCULATION FOR NEURONS
     giERROR(K)= TrainingOutputs(K)-giONEURONS_A
     giODELTA = giONEURONS_S
     DO I=1,giSHIDDEN
        giSDELTA(I)= giSNEURONS_S(I)*giODELTA*arWIOL(I)
     ENDDO 
            
     DO I=1,giFHIDDEN
        giFDELTA(I)= 0
        DO J=1,giSHIDDEN
           giFDELTA(I)= giFDELTA(I)+ &
           giFNEURONS_S(I)*giSDELTA(J)*arWISHL(J,I)
        ENDDO
     ENDDO
             
     !JACOBIAN MATRIX CALCULATION
     DO J=1,giFHIDDEN
        DO I=1,giINPUTS
           JACOBIAN(K,I+(J-1)*giINPUTS)= -1*giFDELTA(J)*TrainingInputs(K,I)
           JACOBIAN(K,J+giFHIDDEN*giINPUTS)= -1*giFDELTA(J) 
        ENDDO
     ENDDO
     L = giFHIDDEN*(giINPUTS+1)
            
     DO J=1,giSHIDDEN
        DO I=1,giFHIDDEN
           JACOBIAN(K,I+(J-1)*giFHIDDEN + L)= -1*giSDELTA(J)*giFNEURONS_A(I)
           JACOBIAN(K,J+giSHIDDEN*giFHIDDEN + L)= -1*giSDELTA(J)
        ENDDO
     ENDDO
     L = giFHIDDEN*(giINPUTS+1)+giSHIDDEN*(giFHIDDEN+1)

     DO I=1,giSHIDDEN
        JACOBIAN(K,I + L)= -1*giODELTA*giSNEURONS_A(I)
     ENDDO
     JACOBIAN(K,giLENGTH)= -1*giODELTA     
 ENDDO

ELSE
 DO K=1,giTRAINPATS
    ! FIRST HIDDEN LAYER
    DO I=1,giFHIDDEN
       giFNEURONS_N(I)= 0
       DO J=1,giINPUTS
          giFNEURONS_N(I)= giFNEURONS_N(I)+ arWIFHL(I,J)*TrainingInputs(K,J) 
       ENDDO
       giFNEURONS_N(I)= giFNEURONS_N(I)+ arBIFHL(I)
       giFNEURONS_A(I)= 1/(1+exp(-giFNEURONS_N(I)))
       giFNEURONS_S(I)= (exp(-giFNEURONS_N(I)))/((1+exp(-giFNEURONS_N(I)))**2)
   ENDDO
            
   !SECOND HIDDEN LAYER
   DO I=1,giSHIDDEN
      giSNEURONS_N(I)= 0
      DO J=1,giFHIDDEN
         giSNEURONS_N(I)= giSNEURONS_N(I)+ arWISHL(I,J)*giFNEURONS_A(J)
      ENDDO
      giSNEURONS_N(I)= giSNEURONS_N(I)+ arBISHL(I)
      giSNEURONS_A(I)= 1/(1+exp(-giSNEURONS_N(I)))
      giSNEURONS_S(I)= (exp(-giSNEURONS_N(I)))/((1+exp(-giSNEURONS_N(I)))**2)
   ENDDO
            
   !THIRD HIDDEN LAYER
   DO I=1,giTHIDDEN
      giTNEURONS_N(I)= 0
      DO J=1,giSHIDDEN
         giTNEURONS_N(I)= giTNEURONS_N(I)+ arWITHL(I,J)*giSNEURONS_A(J)
      ENDDO
      giTNEURONS_N(I)= giTNEURONS_N(I)+ arBITHL(I)
      giTNEURONS_A(I)= 1/(1+exp(-giTNEURONS_N(I)))
      giTNEURONS_S(I)= (exp(-giTNEURONS_N(I)))/((1+exp(-giTNEURONS_N(I)))**2)
   ENDDO
           
   !OUTPUT LAYER
   giONEURONS_N = 0
   DO I=1,giTHIDDEN 
      giONEURONS_N = giONEURONS_N+arWIOL(I)*giTNEURONS_A(I)
   ENDDO
   giONEURONS_N = giONEURONS_N+arBIOL
   giONEURONS_A = 1/(1+exp(-giONEURONS_N))
   giONEURONS_S = (exp(-giONEURONS_N))/((1+exp(-giONEURONS_N))**2)
            
   !ERROR CALCULATION FOR NEURONS
   giERROR(K)= TrainingOutputs(K)-giONEURONS_A
   giODELTA = giONEURONS_S
   DO I=1,giTHIDDEN
      giTDELTA(I)= giTNEURONS_S(I)*giODELTA*arWIOL(I)
   ENDDO 
            
   DO I=1,giSHIDDEN
      giSDELTA(I)= 0
      DO J=1,giTHIDDEN
         giSDELTA(I)= giSDELTA(I)+ &
         giSNEURONS_S(I)*giTDELTA(J)*arWITHL(J,I)
      ENDDO
   ENDDO
            
   DO I=1,giFHIDDEN
      giFDELTA(I)= 0
      DO J=1,giSHIDDEN
         giFDELTA(I)= giFDELTA(I)+ &
         giFNEURONS_S(I)*giSDELTA(J)*arWISHL(J,I)
      ENDDO
   ENDDO
             
   !JACOBIAN MATRIX CALCULATION
   DO J=1,giFHIDDEN
      DO I=1,giINPUTS
         JACOBIAN(K,I+(J-1)*giINPUTS)= -1*giFDELTA(J)*TrainingInputs(K,I)
         JACOBIAN(K,J+giFHIDDEN*giINPUTS)= -1*giFDELTA(J) 
      ENDDO
   ENDDO
   L = giFHIDDEN*(giINPUTS+1)
            
   DO J=1,giSHIDDEN
      DO I=1,giFHIDDEN
         JACOBIAN(K,I+(J-1)*giFHIDDEN + L)= -1*giSDELTA(J)*giFNEURONS_A(I)
         JACOBIAN(K,J+giSHIDDEN*giFHIDDEN + L)= -1*giSDELTA(J)
      ENDDO
   ENDDO
   L = giFHIDDEN*(giINPUTS+1)+giSHIDDEN*(giFHIDDEN+1)
            
   DO J=1,giTHIDDEN
      DO I=1,giSHIDDEN
         JACOBIAN(K,I+(J-1)*giSHIDDEN + L)= -1*giTDELTA(J)*giSNEURONS_A(I)
         JACOBIAN(K,J+giTHIDDEN*giSHIDDEN + L)= -1*giTDELTA(J)
      ENDDO
   ENDDO
   L = giFHIDDEN*(giINPUTS+1)+giSHIDDEN*(giFHIDDEN+1)+giTHIDDEN*(giSHIDDEN+1)

   DO I=1,giTHIDDEN
      JACOBIAN(K,I + L)= -1*giODELTA*giTNEURONS_A(I)
   ENDDO
   JACOBIAN(K,giLENGTH)= -1*giODELTA     
 ENDDO
ENDIF

PRINT *, ' Matrix Jacobian calculated OK!'	
PRINT *, ' '	

 END SUBROUTINE sJacobian
END MODULE JACOBIAN_CalALCULATION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
