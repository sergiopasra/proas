!------------------------------------------------------------------------------
!                                                               File: chlower.f
!------------------------------------------------------------------------------
!omment
!
! SUBROUTINE CHLOWER(CADENA)
!
! Input: CADENA
! Output: CADENA
!
! Upper case characters in CADENA are transformed to lower case
!
! CHARACTER*(*) CADENA -> character string to be transformed
!
!omment
!------------------------------------------------------------------------------
        SUBROUTINE CHLOWER(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
!
        INTEGER I,N
!------------------------------------------------------------------------------
        DO I=1,LEN(CADENA)
          N=ICHAR(CADENA(I:I))
          IF((N.GE.65).AND.(N.LE.90)) CADENA(I:I)=CHAR(N+32)
        END DO
        END
