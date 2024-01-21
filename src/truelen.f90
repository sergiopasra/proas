!------------------------------------------------------------------------------
!                                                               File: truelen.f
!------------------------------------------------------------------------------
!omment
!
! INTEGER FUNCTION TRUELEN(CADENA)
!
! Input: CADENA
! Output: TRUELEN (function)
!
! Return the position of the last non-blank character in CADENA (ignoring also
! control characters with ASCII value < 32)
!
! CHARACTER*(*) CADENA -> input character string
!
!omment
!------------------------------------------------------------------------------
        INTEGER FUNCTION TRUELEN(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
!
        INTEGER I,L
!------------------------------------------------------------------------------
        L=LEN(CADENA)
!
        DO I=L,1,-1
          IF(ICHAR(CADENA(I:I)).GT.32)THEN
            TRUELEN=I
            RETURN
          END IF
        END DO
        TRUELEN=0
        END
