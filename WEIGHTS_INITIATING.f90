
!--------------------------------------------------------------------------
!    THIS SUBROUTINE IS WRITTEN TO INITIATE THE WEIGHTS AND BIASES
!--------------------------------------------------------------------------
 MODULE WEIGHTS_INITIATING
  IMPLICIT NONE
  CONTAINS
  SUBROUTINE sInitiate(giHL, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, &
  arWIFHL, arBIFHL, arWISHL, arBISHL, arWITHL, arBITHL, arWIOL, arBIOL, giLENGTH)
                                         
 ! generate initial random weights and biases
 REAL                     :: rRAND, arBIOL
 INTEGER                  :: I, J, giHL, giLENGTH
 INTEGER, INTENT(IN)      :: giINPUTS, giFHIDDEN
 INTEGER, INTENT(IN)      :: giSHIDDEN, giTHIDDEN
 REAL, ALLOCATABLE        :: arWIFHL(:,:), arBIFHL(:), arWIOL(:)
 REAL, ALLOCATABLE        :: arWISHL(:,:), arBISHL(:) 	
 REAL, ALLOCATABLE        :: arWITHL(:,:), arBITHL(:)

 PRINT *, ' Initiating of weights...'	
 ALLOCATE(arWIFHL(giFHIDDEN,giINPUTS))
 ALLOCATE(arBIFHL(giFHIDDEN))
  
 DO I=1,giFHIDDEN   
    CALL RANDOM_NUMBER(rRAND)
	arBIFHL(I)= (rRAND-0.5)*2
    DO J=1,giINPUTS 
	   CALL RANDOM_NUMBER(rRAND)
	   arWIFHL(I,J)= (rRAND-0.5)*2
	ENDDO
 ENDDO
  
  
 ALLOCATE(arWISHL(giSHIDDEN,giFHIDDEN))
 ALLOCATE(arBISHL(giSHIDDEN))
  
 DO I=1,giSHIDDEN   
	CALL RANDOM_NUMBER(rRAND)
	arBISHL(I)= (rRAND-0.5)*2
    DO J=1,giFHIDDEN 
	   CALL RANDOM_NUMBER(rRAND)
	   arWISHL(I,J)= (rRAND-0.5)*2
   ENDDO
 ENDDO  
  
  
 ALLOCATE(arWITHL(giTHIDDEN,giSHIDDEN))
 ALLOCATE(arBITHL(giTHIDDEN))
  
 DO I=1,giTHIDDEN   
	CALL RANDOM_NUMBER(rRAND)
	arBITHL(I)= (rRAND-0.5)*2
    DO J=1,giSHIDDEN 
	   CALL RANDOM_NUMBER(rRAND)
	   arWITHL(I,J)= (rRAND-0.5)*2
	ENDDO
 ENDDO
  
  
 SELECTCASE(giHL)
 CASE(1)
 ALLOCATE(arWIOL(giFHIDDEN))
 DO I=1,giFHIDDEN   
	CALL RANDOM_NUMBER(rRAND)
	arWIOL(I)= (rRAND-0.5)*2
 ENDDO
 giLENGTH = giFHIDDEN*(giINPUTS+2)+1 
 CASE(2)
 ALLOCATE(arWIOL(giSHIDDEN))
 DO I=1,giSHIDDEN   
	CALL RANDOM_NUMBER(rRAND)
	arWIOL(I)= (rRAND-0.5)*2
 ENDDO
 giLENGTH = giFHIDDEN*(giINPUTS+1) &
 +giSHIDDEN*(giFHIDDEN+2)+1 
 CASE DEFAULT
 ALLOCATE(arWIOL(giTHIDDEN))
 DO I=1,giTHIDDEN   
	CALL RANDOM_NUMBER(rRAND)
	arWIOL(I)= (rRAND-0.5)*2
 ENDDO
 giLENGTH = giFHIDDEN*(giINPUTS+1) &
 +giSHIDDEN*(giFHIDDEN+1) &
 +giTHIDDEN*(giSHIDDEN+2)+1 
 ENDSELECT
  
 CALL RANDOM_NUMBER(rRAND)
 arBIOL = (rRAND-0.5)*2
  
 PRINT *, ' Data initiated OK!'
 PRINT *, ' '
END SUBROUTINE sInitiate
END MODULE WEIGHTS_INITIATING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!