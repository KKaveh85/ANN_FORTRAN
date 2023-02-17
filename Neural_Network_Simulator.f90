

PROGRAM Neural_Network_Simulator
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Neural Network in Fortran90
!! Multilayer Perceptron trained with
!! the Levenberg-Marquardt learning algorithm
!! coded in Fortran90 by Keivan Kaveh
!!-------------------------------------------------------
!! This code reads in data from a text file
!! For the neural network training process follow
!! the code in the subroutine 'sTrain_Net'
!! most of the other code is for the data handling
!!-------------------------------------------------------
!! prefix logic
!! a = array
!! i = integer 
!! r = real
!! l = logical
!! c = character
!! f = function
!! s = subroutine
!! g = global variable
!!-------------------------------------------------------
!!
USE GET_USER_INPUT
USE READ_DATA
USE DATA_SCALING
USE CREATE_TRAINING_DATA
USE WEIGHTS_INITIATING
USE TRAINING
USE TEST_PREDICTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE

!--------------------------------------------------------------------------
!                              DECALARATIONS
!--------------------------------------------------------------------------
INTEGER              :: giPATS, giTRAINPATS, giTESTPATS, giVALIDPATS 
INTEGER              :: giINPUTS, giHL, giFHIDDEN, giSHIDDEN, giTHIDDEN
INTEGER              :: giLENGTH, iSTOP, iFINDER, iEpch, iEpchMAX
REAL                 :: arBIOL, arBIOL_NEW, arBIOL_Best, rSSE_OLD
REAL                 :: rSSE_NEW, rSSE_VAL, rSSE_VALBest, giCC, grMinOut, grMaxOut 
REAL, ALLOCATABLE    :: All_Inputs(:,:), Scaled_Input(:,:), All_Targets(:) 
REAL, ALLOCATABLE    :: Scaled_Target(:), TrainingInputs(:,:), TrainingOutputs(:)
REAL, ALLOCATABLE    :: TestingInputs(:,:), TestingOutputs(:), ValidationInputs(:,:)
REAL, ALLOCATABLE    :: ValidationOutputs(:), arTestPREDICTED(:), arValPREDICTED(:)
REAL, ALLOCATABLE    :: JACOBIAN(:,:), JACOBIAN_TRANSPOSE(:,:), VAL_ERROR(:), giERROR(:)
REAL, ALLOCATABLE    :: HESSIAN(:,:) ,  giHESSIAN_INVERSE(:,:), arInputPREDICTED(:)
REAL, ALLOCATABLE    :: arWIFHL(:,:), arWIFHL_NEW(:,:), arWIFHL_Best(:,:)             
REAL, ALLOCATABLE    :: arWISHL(:,:), arWISHL_NEW(:,:), arWISHL_Best(:,:)    
REAL, ALLOCATABLE    :: arWITHL(:,:), arWITHL_NEW(:,:), arWITHL_Best(:,:)
REAL, ALLOCATABLE    :: arBIFHL(:), arBIFHL_NEW(:), arBIFHL_Best(:)
REAL, ALLOCATABLE    :: arBISHL(:), arBISHL_NEW(:), arBISHL_Best(:) 
REAL, ALLOCATABLE    :: arBITHL(:), arBITHL_NEW(:), arBITHL_Best(:)
REAL, ALLOCATABLE    :: arWIOL(:) , arWIOL_NEW(:) ,  arWIOL_Best(:)
REAL, ALLOCATABLE    :: TEST_TARGET(:), TEST_PREDICTED(:)
REAL, ALLOCATABLE    :: ALL_TARGET(:) , ALL_PREDICTED(:)
REAL, ALLOCATABLE    :: VALIDATION_TARGET(:), VALIDATION_PREDICTED(:)
CHARACTER(LEN = *), PARAMETER :: FMTHEAD = '(3X, A, 2(3X, A))'
CHARACTER(LEN = *), PARAMETER :: FMDATA  = '(2(X, F14.10))'
CHARACTER(LEN = *), PARAMETER :: FMDATA2 = '(2(X, 5X, F14.10))'

!--------------------------------------------------------------------
!                        CALLING SUBROUTINES
!--------------------------------------------------------------------
 CALL sGET(giINPUTS, giHL, giFHIDDEN, giSHIDDEN, giTHIDDEN, giCC, &
      iEpchMAX, iSTOP) 

 CALL sREAD(giINPUTS, giPATS, All_Inputs, All_Targets)

 CALL sScale(giPATS, giINPUTS, All_Inputs, Scaled_Input, All_Targets, &
      Scaled_Target, grMinOut, grMaxOut)

 CALL sCreate(giPATS, giINPUTS, giTRAINPATS, giVALIDPATS, giTESTPATS,   &
      Scaled_Input, Scaled_Target, TrainingInputs, TrainingOutputs,     &
      ValidationInputs, ValidationOutputs, TestingInputs, TestingOutputs)

 CALL sInitiate(giHL, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, arWIFHL,  &
      arBIFHL, arWISHL, arBISHL, arWITHL, arBITHL, arWIOL, arBIOL, giLENGTH)
  
   
 CALL sTrain(giINPUTS, giHL, giTRAINPATS, giVALIDPATS, giLENGTH, giFHIDDEN,&
      giSHIDDEN, giTHIDDEN, iEpch, iEpchMAX, arWIFHL, arBIFHL, arWISHL,    &
      arBISHL, arWITHL, arBITHL, arWIOL, arBIOL, giERROR, JACOBIAN, iSTOP, &
      JACOBIAN_TRANSPOSE, HESSIAN, giHESSIAN_INVERSE, TrainingInputs,      &
      TrainingOutputs, arWIFHL_NEW, ValidationInputs, ValidationOutputs,   &
      arBIFHL_NEW, arWISHL_NEW, arBISHL_NEW, arWITHL_NEW, arBITHL_NEW,     &
      arWIOL_NEW, arBIOL_NEW, arWIFHL_Best, arBIFHL_Best, arWISHL_Best,    &
      arBISHL_Best, arWITHL_Best, arBITHL_Best, arWIOL_Best, arBIOL_Best,  &
      rSSE_OLD, rSSE_NEW, rSSE_VAL, rSSE_VALBest, VAL_ERROR, iFINDER, giCC)
   
   
 CALL sTEST(giHL, giTESTPATS, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, &
      TestingInputs, arTestPREDICTED, arWIFHL_Best, arBIFHL_Best,        &
      arWISHL_Best, arBISHL_Best, arWITHL_Best, arBITHL_Best, arWIOL_Best, &
      arBIOL_Best, grMaxOut, grMinOut, TestingOutputs, TEST_TARGET, TEST_PREDICTED)

      
 CALL sTEST(giHL, giVALIDPATS, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, &
      ValidationInputs, arValPREDICTED, arWIFHL_Best, arBIFHL_Best,       &
      arWISHL_Best, arBISHL_Best, arWITHL_Best, arBITHL_Best, arWIOL_Best,& 
      arBIOL_Best, grMaxOut, grMinOut, ValidationOutputs, VALIDATION_TARGET, VALIDATION_PREDICTED)
      
      
 CALL sTEST(giHL, giPATS, giINPUTS, giFHIDDEN, giSHIDDEN, giTHIDDEN, &
      Scaled_Input, arInputPREDICTED, arWIFHL_Best, arBIFHL_Best,    &
      arWISHL_Best, arBISHL_Best, arWITHL_Best, arBITHL_Best, arWIOL_Best,&
      arBIOL_Best, grMaxOut, grMinOut, All_Targets, ALL_TARGET, ALL_PREDICTED)


 OPEN (unit = 66, file = "C:\Keivan\Machine_Learning\Artificial_Neural_Networks\FORTRAN\ANN_Console\MY_RESULTS\TEST_RESULTS.txt", action = "write", status = "replace")
 WRITE (66, FMTHEAD) 'TEST_TARGET', 'TEST_PREDICTED'
 WRITE (66, FMDATA) TEST_TARGET, TEST_PREDICTED
 
 OPEN (unit = 77, file = "C:\Keivan\Machine_Learning\Artificial_Neural_Networks\FORTRAN\ANN_Console\MY_RESULTS\VALIDATION_RESULTS.txt", action = "write", status = "replace")
 WRITE (77, FMTHEAD) 'VALIDATION_TARGET', 'VALIDATION_PREDICTED'
 WRITE (77, FMDATA2) VALIDATION_TARGET, VALIDATION_PREDICTED
 
 OPEN (unit = 88, file = "C:\Keivan\Machine_Learning\Artificial_Neural_Networks\FORTRAN\ANN_Console\MY_RESULTS\ALL_RESULTS.txt", action = "write", status = "replace")
 WRITE (88, FMTHEAD) 'ALL_TARGET', 'ALL_PREDICTED'
 WRITE (88, FMDATA) ALL_TARGET, ALL_PREDICTED

END PROGRAM Neural_Network_Simulator
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!