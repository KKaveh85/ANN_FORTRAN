
!--------------------------------------------------------------------------
!           THIS SUBROUTINE IS WRITTEN TO PREVENT OVERFITTING
!--------------------------------------------------------------------------
 MODULE EARLY_STOPPING
  IMPLICIT NONE
  CONTAINS
  
  SUBROUTINE sEARLY_STOPPING(iEpch, ERROR, iSTOP, iFINDER)
  INTEGER           :: I, iFINDER, iEpch, iSTOP
  REAL, ALLOCATABLE :: ERROR(:)
  
  iFINDER = 0
  DO I=1,iSTOP
     IF(ERROR(iEpch-I+1) < ERROR(iEpch-I)) THEN
       iFINDER = 1
     ENDIF
  ENDDO
 
  END SUBROUTINE sEARLY_STOPPING
 END MODULE EARLY_STOPPING
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!