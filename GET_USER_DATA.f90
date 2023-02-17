
!--------------------------------------------------------------------
!           THIS SUBROUTINE IS WRITTEN TO GET USER INPUT
!--------------------------------------------------------------------
 MODULE GET_USER_INPUT
  IMPLICIT NONE
  CONTAINS
  
  SUBROUTINE sGET(giINPUTS, giHL, giFHIDDEN, giSHIDDEN, giTHIDDEN, giCC, &
  iEpchMAX, iSTOP)
  ! getting user input
  INTEGER, INTENT(INOUT) :: giINPUTS, giHL, giFHIDDEN
  INTEGER, INTENT(INOUT) :: giSHIDDEN, giTHIDDEN, iEpchMAX, iSTOP
  REAL, INTENT(INOUT)    :: giCC
  
  PRINT *, ' '
  PRINT *, ' Getting user data...'
  WRITE(*,'(A)',advance='no') '  Please enter the number of input variables: '
  READ *, giINPUTS

  WRITE(*,'(A)',advance='no') '  How many hidden layers[1-3]? '
  READ *, giHL
		
  IF(giHL < 1) THEN
	 PRINT *, '  Hidden Layers set to 1'
     giHL = 1
  ELSEIF(giHL > 3) THEN
	 PRINT *, '  Hidden Layers set to 3'
     giHL = 3
  ENDIF
		
  WRITE(*,'(A)',advance='no') '  How many neurons for the first hidden layer? '
  READ *, giFHIDDEN
  giSHIDDEN = 0
  giTHIDDEN = 0
  IF(giHL == 1) GOTO 10
  WRITE(*,'(A)',advance='no') '  How many neurons for the second hidden layer? '
  READ *, giSHIDDEN 
  giTHIDDEN = 0 
  IF(giHL == 2) GOTO 10
  WRITE(*,'(A)',advance='no') '  How many neurons for the third hidden layer? '
  READ *, giTHIDDEN 

10 WRITE(*,'(A)',advance='no') '  Please enter the value of combination coefficient: '
   READ *, giCC
   
   WRITE(*,'(A)',advance='no') '  How many iterations? '
   READ *, iEpchMAX
   
   WRITE(*,'(A)',advance='no') '  Maximum number of validation fails? '
   READ *, iSTOP
   
   PRINT *, '  Data getting OK!'
   PRINT *, ' '  
 END SUBROUTINE sGET
END MODULE GET_USER_INPUT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!