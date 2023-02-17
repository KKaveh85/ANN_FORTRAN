 
!--------------------------------------------------------------------------
!       THIS SUBROUTINE IS WRITTEN TO ADJUST THE WEIGHTS AND BIASES
!--------------------------------------------------------------------------
 MODULE WEIGHTS_ADJUSTING
  IMPLICIT NONE
  CONTAINS
  
  SUBROUTINE sWeight(giINPUTS, giLENGTH, giTRAINPATS, giFHIDDEN, giSHIDDEN, &
  giTHIDDEN, giHL, giHESSIAN_INVERSE, JACOBIAN_TRANSPOSE, giERROR, arWIFHL, &
  arBIFHL, arWISHL, arBISHL, arWITHL, arBITHL, arWIOL, arBIOL, arWIFHL_NEW, &
  arBIFHL_NEW, arWISHL_NEW, arBISHL_NEW, arWITHL_NEW, arBITHL_NEW,arWIOL_NEW, arBIOL_NEW) 
  
  INTEGER             :: I, J, K, L 
  INTEGER, INTENT(IN) :: giINPUTS, giLENGTH, giTRAINPATS
  INTEGER, INTENT(IN) :: giFHIDDEN, giSHIDDEN, giTHIDDEN, giHL
  REAL                :: arBIOL, arBIOL_NEW
  REAL, ALLOCATABLE   :: arHJT(:,:), DW(:), arWIOL_NEW(:), arWIOL(:)    
  REAL, ALLOCATABLE   :: giHESSIAN_INVERSE(:,:), JACOBIAN_TRANSPOSE(:,:)
  REAL, ALLOCATABLE   :: giERROR(:), arWIFHL(:,:), arBIFHL(:) 
  REAL, ALLOCATABLE   :: arWISHL(:,:), arBISHL(:), arWITHL(:,:), arBITHL(:)
  REAL, ALLOCATABLE   :: arWIFHL_NEW(:,:), arBIFHL_NEW(:)
  REAL, ALLOCATABLE   :: arWISHL_NEW(:,:), arBISHL_NEW(:)
  REAL, ALLOCATABLE   :: arWITHL_NEW(:,:), arBITHL_NEW(:)
  
  PRINT *, ' Weight adjusting ...'
  ALLOCATE(arHJT(giLENGTH,giTRAINPATS))
  ALLOCATE(DW(giLENGTH))
  ALLOCATE(arWIFHL_NEW(giFHIDDEN,giINPUTS))
  ALLOCATE(arBIFHL_NEW(giFHIDDEN))
  ALLOCATE(arWISHL_NEW(giSHIDDEN,giFHIDDEN))
  ALLOCATE(arBISHL_NEW(giSHIDDEN))
  ALLOCATE(arWITHL_NEW(giTHIDDEN,giSHIDDEN))
  ALLOCATE(arBITHL_NEW(giTHIDDEN)) 
  
  DO J=1,giLENGTH
    DO K=1,giTRAINPATS
       arHJT(J,K)= 0
       DO I=1,giLENGTH
          arHJT(J,K)= arHJT(J,K)+ giHESSIAN_INVERSE(J,I) &
          *JACOBIAN_TRANSPOSE(I,K)
       ENDDO
    ENDDO
  ENDDO

  DO J=1,giLENGTH
     DW(J)= 0
     DO I=1,giTRAINPATS
        DW(J)= DW(J)+ arHJT(J,I)*giERROR(I)
     ENDDO
  ENDDO
  
  
 IF(giHL==1) THEN
   ALLOCATE(arWIOL_NEW(giFHIDDEN))
    DO I=1,giFHIDDEN
     DO J=1,giINPUTS
        arWIFHL_NEW(I,J)= arWIFHL(I,J)- DW((I-1)*giINPUTS+J)
     ENDDO
     arBIFHL_NEW(I)= arBIFHL(I)- DW(giFHIDDEN*giINPUTS+I)
    ENDDO 
    
    L = giFHIDDEN*(giINPUTS+1)
    arWIOL_NEW(:)= arWIOL(:)- DW(L+1:L+giFHIDDEN)
    arBIOL_NEW = arBIOL - DW(giLENGTH)
    
 ELSEIF(giHL==2) THEN
   ALLOCATE(arWIOL_NEW(giSHIDDEN))
    DO I=1,giFHIDDEN
     DO J=1,giINPUTS
        arWIFHL_NEW(I,J)= arWIFHL(I,J)- DW((I-1)*giINPUTS+J)
     ENDDO
     arBIFHL_NEW(I)= arBIFHL(I)- DW(giFHIDDEN*giINPUTS+I)
    ENDDO 
    L = giFHIDDEN*(giINPUTS+1)
    
    DO I=1,giSHIDDEN
     DO J=1,giFHIDDEN
        arWISHL_NEW(I,J)= arWISHL(I,J)- DW(L+(I-1)*giFHIDDEN+J)
     ENDDO
     arBISHL_NEW(I)= arBISHL(I)- DW(L+giSHIDDEN*giFHIDDEN+I)
    ENDDO
    L = giFHIDDEN*(giINPUTS+1)+giSHIDDEN*(giFHIDDEN+1)
    arWIOL_NEW(:)= arWIOL(:)- DW(L+1:L+giSHIDDEN)
    arBIOL_NEW = arBIOL - DW(giLENGTH)
    
 ELSE
   ALLOCATE(arWIOL_NEW(giTHIDDEN))
    DO I=1,giFHIDDEN
     DO J=1,giINPUTS
        arWIFHL_NEW(I,J)= arWIFHL(I,J)- DW((I-1)*giINPUTS+J)
     ENDDO
     arBIFHL_NEW(I)= arBIFHL(I)- DW(giFHIDDEN*giINPUTS+I)
   ENDDO 
   L = giFHIDDEN*(giINPUTS+1)
    
   DO I=1,giSHIDDEN
    DO J=1,giFHIDDEN
       arWISHL_NEW(I,J)= arWISHL(I,J)- DW(L+(I-1)*giFHIDDEN+J)
    ENDDO
    arBISHL_NEW(I)= arBISHL(I)- DW(L+giSHIDDEN*giFHIDDEN+I)
   ENDDO
   L = giFHIDDEN*(giINPUTS+1)+giSHIDDEN*(giFHIDDEN+1)
    
   DO I=1,giTHIDDEN
    DO J=1,giSHIDDEN
       arWITHL_NEW(I,J)= arWITHL(I,J)- DW(L+(I-1)*giSHIDDEN+J)
    ENDDO
    arBITHL_NEW(I)= arBITHL(I)- DW(L+giTHIDDEN*giSHIDDEN+I)
   ENDDO
   L = giFHIDDEN*(giINPUTS+1)+giSHIDDEN*(giFHIDDEN+1)+giTHIDDEN*(giSHIDDEN+1)
   arWIOL_NEW(:)= arWIOL(:)- DW(L+1:L+giTHIDDEN)
   arBIOL_NEW = arBIOL - DW(giLENGTH) 
 ENDIF
   
  PRINT *, ' Weights and biases adjusted OK!'
  PRINT *, ' '
  END SUBROUTINE sWeight
 END MODULE WEIGHTS_ADJUSTING
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!