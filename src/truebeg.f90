!------------------------------------------------------------------------------
!                                                               File: truebeg.f
!------------------------------------------------------------------------------
!omment
!
! INTEGER FUNCTION TRUEBEG(CADENA)
!
! Input: CADENA
! Output: TRUEBEG (function)
!
! Return the position of the first non-blank character in CADENA (ignoring
! also control characters with ASCII value < 32)
!
! CHARACTER*(*) CADENA -> input character string
!
!omment
!------------------------------------------------------------------------------
        INTEGER FUNCTION TRUEBEG(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
!
        INTEGER I,L
!------------------------------------------------------------------------------
        L=LEN(CADENA)
!
        DO I=1,L
          IF(ICHAR(CADENA(I:I)).GT.32)THEN
            TRUEBEG=I
            RETURN
          END IF
        END DO
        TRUEBEG=0
        END
