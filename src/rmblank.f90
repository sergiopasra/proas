!------------------------------------------------------------------------------
!                                                               File: rmblank.f
!------------------------------------------------------------------------------
!omment
!
! SUBROUTINE RMBLANK(C1,C2,L)
!
! Input: C1
! Output: C2,L
!
! Remove blanks in character string C1, returning C2 with a true length L
!
! CHARACTER*(*) C1 -> input character string
! CHARACTER*(*) C2 -> output character string (C1 without blanks)
! INTEGER       L -> true len of C2
!
!omment
!------------------------------------------------------------------------------
        SUBROUTINE RMBLANK(C1,C2,L)
        IMPLICIT NONE
        INTEGER L
        CHARACTER*(*) C1,C2
!
        INTEGER I,K
!------------------------------------------------------------------------------
        K=0
        DO I=1,LEN(C1)
          IF(C1(I:I).NE.CHAR(32))THEN
            K=K+1
            C2(K:K)=C1(I:I)
          END IF
        END DO
        L=K
        END
