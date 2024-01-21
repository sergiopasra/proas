! **********************************************************************
!                                                     SUBROUTINE TIEMPOU
!                                                     ******************
!
      SUBROUTINE TIEMPOU(A,H,L,TS0,TU)
!
! Calculo del Tiempo Universal.
! Datos: A.R., h, Longitud, TSG0
!
      IMPLICIT NONE
!---> argumentos ficticios: ENTRADA
      REAL A,H,L,TS0
!---> argumentos ficticios: SALIDA
      REAL TU
!---> variables locales
      REAL TSL,TSG
      REAL DTS
!
      TSL=A+H
      IF(TSL.GT.24.)TSL=TSL-24.
      TSG=TSL-L
      IF(TSG.GT.24.)TSG=TSG-24.
      IF(TSG.LT.0.)TSG=TSG+24.
      DTS=TSG-TS0
      IF(DTS.LT.0.)DTS=DTS+24.
      TU=.997270*DTS
      IF(TU.LT.0.)TU=TU+24.
      END
