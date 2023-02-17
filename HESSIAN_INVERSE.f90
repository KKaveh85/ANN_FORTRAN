
!--------------------------------------------------------------------------
!  THIS SUBROUTINE IS WRITTEN TO CALCULATE THE INVERSE OF HESSIAN MATRIX
!--------------------------------------------------------------------------
 MODULE HESSIAN_INVERSE
  USE MATRIX_INVERSION
  IMPLICIT NONE
  CONTAINS
 
  SUBROUTINE sHessian(giLENGTH, giTRAINPATS, JACOBIAN, JACOBIAN_TRANSPOSE, &
  giCC, HESSIAN, giHESSIAN_INVERSE)
  
  INTEGER           :: I, J, K
  INTEGER           :: giLENGTH, giTRAINPATS
  REAL              :: giCC
  REAL, ALLOCATABLE :: JACOBIAN_TRANSPOSE(:,:), JACOBIAN(:,:)
  REAL, ALLOCATABLE :: giIDENTITY(:,:), giJTJ(:,:), HESSIAN(:,:)
  REAL, ALLOCATABLE :: giHESSIAN_INVERSE(:,:)
  
  PRINT *, ' Hessian matrix calculation ...'
  ALLOCATE(giJTJ(giLENGTH, giLENGTH))
  ALLOCATE(HESSIAN(giLENGTH, giLENGTH))
  ALLOCATE(giIDENTITY(giLENGTH, giLENGTH))
  ALLOCATE(JACOBIAN_TRANSPOSE(giLENGTH, giTRAINPATS))
  
  JACOBIAN_TRANSPOSE = TRANSPOSE(JACOBIAN)
  !Identity matrix creation
  DO I=1,giLENGTH
    DO J=1,giLENGTH
       IF(I==J) THEN
          giIDENTITY(I,J)= 1
       ELSE
          giIDENTITY(I,J)= 0
       ENDIF
    ENDDO
  ENDDO
  
  DO J=1,giLENGTH
    DO K=1,giLENGTH
       giJTJ(J,K)= 0
       DO I=1,giTRAINPATS
          giJTJ(J,K)= giJTJ(J,K)+ &
          JACOBIAN_TRANSPOSE(J,I)*JACOBIAN(I,K)
       ENDDO
    ENDDO
  ENDDO
  HESSIAN = giJTJ + giCC*giIDENTITY
 
  CALL sMatrix_Inverse(giLENGTH, HESSIAN, giHESSIAN_INVERSE)
 
  PRINT *, ' Hessian inversion calculated OK!'
  PRINT *, ' ' 
  END SUBROUTINE sHessian
 END MODULE HESSIAN_INVERSE
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!