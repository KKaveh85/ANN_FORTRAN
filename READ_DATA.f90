
!--------------------------------------------------------------------------
!   THIS SUBROUTINE IS WRITTEN TO READ INPUT AND TARGET FROM TXT FILES
!--------------------------------------------------------------------------
 MODULE READ_DATA
  IMPLICIT NONE
  CONTAINS
  
  SUBROUTINE sREAD(giINPUTS, giPATS, All_Inputs, All_Targets)
  ! read in data
  INTEGER              :: I, IO, giPATS, giINPUTS
  INTEGER, PARAMETER   :: giUNIT = 10
  INTEGER, ALLOCATABLE :: pair(:)
  REAL, ALLOCATABLE    :: All_Inputs(:,:)
  REAL, ALLOCATABLE    :: All_Targets(:)
   
  PRINT *, ' Reading data...'

  ALLOCATE(pair(giINPUTS))
  OPEN(giUNIT, file="C:\Keivan\Machine_Learning\Artificial_Neural_Networks\FORTRAN\ANN_Console\MY_INPUT\My_Input.txt") 
  giPATS = 0
  DO
    READ(giUNIT, *, iostat=IO) pair
    IF (IO/=0) EXIT
    giPATS = giPATS + 1
  ENDDO

  REWIND(giUNIT)
  ALLOCATE(All_Inputs(giPATS, giINPUTS))

  DO I=1,giPATS
     READ(giUNIT, *) All_Inputs(I,:)
  ENDDO
  CLOSE(giUNIT)

  OPEN(giUNIT, file="C:\Keivan\Machine_Learning\Artificial_Neural_Networks\FORTRAN\ANN_Console\MY_INPUT\My_Target.txt") 
  ALLOCATE(All_Targets(giPATS))
  READ(giUNIT, *) All_Targets(:)
  CLOSE(giUNIT)
  
  PRINT *, ' Data read OK!'
  PRINT *, ' '
 END SUBROUTINE sREAD
END MODULE READ_DATA 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!