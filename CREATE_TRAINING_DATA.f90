
!--------------------------------------------------------------------------
! THIS SUBROUTINE IS WRITTEN TO CREAT TRAIN, TEST AND VALIDATION DATA SETS
!--------------------------------------------------------------------------
 MODULE CREATE_TRAINING_DATA
  IMPLICIT NONE
  CONTAINS
   
  SUBROUTINE sCreate(giPATS, giINPUTS, giTRAINPATS, giVALIDPATS, giTESTPATS,&
  garInArray, garTarArray, garTrainingInputs, garTrainingOutputs, &
  garValidationInputs, garValidationOutputs, garTestingInputs, garTestingOutputs)
  ! create train, test and validation sets

  INTEGER              :: I, J, Index, Counter, a, b, c
  INTEGER              :: giTRAINPATS , giTESTPATS, giVALIDPATS
  INTEGER, INTENT(IN)  :: giPATS, giINPUTS
  INTEGER              :: My_Array(giPATS), Shuffle(giPATS)
  REAL                 :: rRAND
  REAL                 :: garTarArray(giPATS) 
  REAL                 :: garInArray(giPATS,giINPUTS) 
  REAL, ALLOCATABLE    :: garTrainingInputs(:,:), garTrainingOutputs(:)
  REAL, ALLOCATABLE    :: garTestingInputs(:,:), garTestingOutputs(:)
  REAL, ALLOCATABLE    :: garValidationInputs(:,:), garValidationOutputs(:) 	
		
  PRINT *, ' Allocating data...'	
	
  DO I = 1,giPATS
     My_Array(I)= I
  ENDDO
  
  Counter = 0
  DO I=giPATS,1,-1
     Counter = Counter + 1
     CALL RANDOM_SEED
     CALL RANDOM_NUMBER(rRAND)
     Index = FLOOR(rRAND*I)+1
     Shuffle(Counter)= My_array(Index+Counter-1)
     My_Array(Index+Counter-1)= My_Array(Counter)
  ENDDO
   
  a = FLOOR(0.7*(Counter))
  giTRAINPATS = a
  ALLOCATE(garTrainingInputs(giTRAINPATS,giINPUTS))
  ALLOCATE(garTrainingOutputs(giTRAINPATS))
   
  DO I=1,giTRAINPATS
     Index = Shuffle(I)
     garTrainingInputs(I,1:giINPUTS)= garInArray(Index,1:giINPUTS)
     garTrainingOutputs(I)= garTarArray(Index)
  ENDDO
   
  b = FLOOR(0.85*(Counter))
  giVALIDPATS = b-a
  ALLOCATE(garValidationInputs(giVALIDPATS,giINPUTS))
  ALLOCATE(garValidationOutputs(giVALIDPATS))
   
  J = 0
  DO I=a+1,b
     J = J+1
     Index = Shuffle(I)
     garValidationInputs(J,1:giINPUTS)= garInArray(Index,1:giINPUTS)
     garValidationOutputs(J)= garTarArray(Index)
  ENDDO 

  c = Counter
  giTESTPATS = c-b
  ALLOCATE(garTestingInputs(giTESTPATS,giINPUTS))
  ALLOCATE(garTestingOutputs(giTESTPATS))
   
  J = 0
  DO I=b+1,c
     J = J+1
     Index = Shuffle(I)
     garTestingInputs(J,1:giINPUTS)= garInArray(Index,1:giINPUTS)       
     garTestingOutputs(J)= garTarArray(Index)
  ENDDO
   
  PRINT *, ' Data allocated OK!'
  PRINT *, ' '
 END SUBROUTINE sCreate
END MODULE CREATE_TRAINING_DATA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!