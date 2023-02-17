
!--------------------------------------------------------------------------
!     THIS SUBROUTINE IS WRITTEN TO COMPUTE INVERSE MATRIX
      !    Method: Based on the Doolittle LU method       !
      ! step 0: initialization for matrices L and U and b !
      !  Fortran 90/95 aloows such operations on matrices !
!--------------------------------------------------------------------------
MODULE MATRIX_INVERSION
 IMPLICIT NONE
 CONTAINS
 SUBROUTINE sMatrix_Inverse(giLENGTH, AA, giINVERSE)
  
  INTEGER           :: I, J, K
  INTEGER           :: giLENGTH
  REAL              :: COEFF
  REAL, ALLOCATABLE :: L(:,:), U(:,:), giINVERSE(:,:)
  REAL, ALLOCATABLE :: D(:), E(:), F(:), AA(:,:)  
  
  PRINT *, ' Inversing data...'
  ALLOCATE(D(giLENGTH))
  ALLOCATE(E(giLENGTH))
  ALLOCATE(F(giLENGTH))
  ALLOCATE(L(giLENGTH, giLENGTH)) 
  ALLOCATE(U(giLENGTH, giLENGTH))
  ALLOCATE(giINVERSE(giLENGTH, giLENGTH))
  
  L = 0.0
  U = 0.0
  D = 0.0
  ! step 1: forward elimination
  DO K=1,giLENGTH-1
    DO I=K+1,giLENGTH
       COEFF = AA(I,K)/AA(K,K)
       L(I,K) = COEFF
       DO J=K+1,giLENGTH
          AA(I,J)= AA(I,J)-COEFF*AA(K,J)
       ENDDO
    ENDDO
  ENDDO

  ! Step 2: prepare L and U matrices 
  ! L matrix is a matrix of the elimination coefficient
  ! + the diagonal elements are 1.0
  DO I=1,giLENGTH
     L(I,I) = 1.0
  ENDDO
     
  DO J=1,giLENGTH
     DO I=1,J
        U(I,J)= AA(I,J)
     ENDDO
  ENDDO

  ! Step 3:
  DO K=1,giLENGTH
     D(K)= 1.0
     E(1)= D(1)
     DO I=2,giLENGTH
        E(I) = D(I)
        DO J=1,I-1
           E(I)= E(I) - L(I,J)*E(J)
        ENDDO
     ENDDO
     F(giLENGTH)= E(giLENGTH)/U(giLENGTH,giLENGTH)
     DO I=giLENGTH-1,1,-1
        F(I)= E(I)
        DO J=giLENGTH,I+1,-1
           F(I)= F(I)-U(I,J)*F(J)
        ENDDO
        F(I)= F(I)/U(I,I)
     ENDDO
     DO I=1,giLENGTH
        giINVERSE(I,K) = F(I)
     ENDDO
     D(K)=0.0
  ENDDO
  
  PRINT *, ' Data inversion OK!'
  PRINT *, ' ' 
 END SUBROUTINE sMatrix_Inverse
END MODULE MATRIX_INVERSION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11