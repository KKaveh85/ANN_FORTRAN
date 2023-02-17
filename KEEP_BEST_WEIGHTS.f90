 !--------------------------------------------------------------------------
!            THIS SUBROUTINE IS WRITTEN TO KEEP BEST WEIGHTS
!--------------------------------------------------------------------------
MODULE KEEP_BEST_WEIGHTS
 IMPLICIT NONE
 CONTAINS
 SUBROUTINE sKEEP(iEpch, giHL, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, &
 rSSE_VAL, rSSE_VALBest, arBIFHL, arBIFHL_Best, arBISHL, arBISHL_Best,    &
 arBITHL, arBITHL_Best, arWIFHL_Best, arWIFHL, arWISHL_Best, arWISHL,     &
 arWITHL_Best, arWITHL, arWIOL, arWIOL_Best,arBIOL, arBIOL_Best)
 ! if the overall error has improved then keep the new weights
 
 INTEGER, INTENT(IN) :: iEpch, giHL
 INTEGER, INTENT(IN) :: giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN
 REAL                :: arBIOL, arBIOL_Best, rSSE_VAL, rSSE_VALBest
 REAL, ALLOCATABLE   :: arBIFHL(:), arBIFHL_Best(:)
 REAL, ALLOCATABLE   :: arBISHL(:), arBISHL_Best(:)
 REAL, ALLOCATABLE   :: arBITHL(:), arBITHL_Best(:)
 REAL, ALLOCATABLE   :: arWIFHL_Best(:,:), arWIFHL(:,:)
 REAL, ALLOCATABLE   :: arWISHL_Best(:,:), arWISHL(:,:)
 REAL, ALLOCATABLE   :: arWITHL_Best(:,:), arWITHL(:,:)
 REAL, ALLOCATABLE   :: arWIOL(:), arWIOL_Best(:) 
    
  ALLOCATE(arWIFHL_Best(giFHIDDEN,giINPUTS))
  ALLOCATE(arWISHL_Best(giSHIDDEN,giFHIDDEN))
  ALLOCATE(arWITHL_Best(giTHIDDEN,giSHIDDEN))
  ALLOCATE(arBIFHL_Best(giFHIDDEN))
  ALLOCATE(arBISHL_Best(giSHIDDEN))
  ALLOCATE(arBITHL_Best(giTHIDDEN))
    
    IF(giHL .EQ. 1) THEN
      ALLOCATE(arWIOL_Best(giFHIDDEN))
    ELSEIF(giHL .EQ. 2) THEN
      ALLOCATE(arWIOL_Best(giSHIDDEN))
    ELSE
      ALLOCATE(arWIOL_Best(giTHIDDEN))
    ENDIF
    
	!this will be on the first epoch
	IF (iEpch .EQ. 1) THEN
		rSSE_VALBest = rSSE_VAL
	ENDIF

	IF(rSSE_VAL <= rSSE_VALBest) THEN
	   arWIFHL_Best = arWIFHL
	   arBIFHL_Best = arBIFHL
	   arWISHL_Best = arWISHL
	   arBISHL_Best = arBISHL
	   arWITHL_Best = arWITHL
	   arBITHL_Best = arBITHL
	   arWIOL_Best = arWIOL
	   arBIOL_Best = arBIOL
	   rSSE_VALBest = rSSE_VAL 
	ENDIF


 END SUBROUTINE sKEEP
END MODULE KEEP_BEST_WEIGHTS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!