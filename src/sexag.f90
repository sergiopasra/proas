! **********************************************************************
!                                                       SUBROUTINE SEXAG
!                                                       ****************
!
      SUBROUTINE SEXAG(A,B,C,D)
!
! Paso de DD.dddd a DD.MMSS
!
      IMPLICIT NONE
!---> argumentos ficticios: ENTRADA
      REAL A
!---> argumentos ficticios: SALIDA
      REAL B,C,D
!---> variables locales
      REAL AA,BR
!
      AA=ABS(A)
      B=AINT(AA)
      BR=(AA-B)*60.
      C=AINT(BR)
      D=ANINT((BR-AINT(BR))*60)
      IF(D.GE.60.)THEN
        C=C+1.
        D=0.
        IF(C.GE.60.)THEN
          B=B+1.
          C=0.
        END IF
      END IF
! hay que tener cuidado con la siguiente asignacion de signos
! (necesaria por si B=0 o/y C=0)
      IF(A.LT.0.)THEN
        B=-B
        C=-C
        D=-D
      END IF
      END
