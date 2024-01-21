! **********************************************************************
!                                                    SUBROUTINE CAMBCOOR
!                                                    *******************
!
      SUBROUTINE CAMBCOOR(HOR,DEC,ACI,ALT)
!
! Transformaciones de coordenadas:
! (angulo horario,declinacion) ----> (acimut,altura)
!
      IMPLICIT NONE
!---> parametros
      REAL PI
      PARAMETER(PI=3.141593)
!---> argumentos ficticios: ENTRADA
      REAL HOR,DEC
!---> argumentos ficticios: SALIDA
      REAL ACI,ALT
!---> variables globales
      REAL LAT,LONG
!---> variables locales
      REAL HORR,DECR
      REAL LATR
      REAL SENOAC,COSEAC
!---> common blocks
      COMMON/BLKOBS/LAT,LONG
!
      HORR=HOR*PI/180.*15.
      DECR=DEC*PI/180.
      LATR=LAT*PI/180.
      SENOAC=COS(DECR)*SIN(HORR)
      COSEAC=-(SIN(DECR)*COS(LATR))+COS(DECR)*SIN(LATR)*COS(HORR)
      ACI=ATAN2(SENOAC,COSEAC)
      ACI=ACI*180./PI
      IF(ACI.LT.0.)ACI=ACI+360.
      ALT=ASIN(SIN(LATR)*SIN(DECR)+COS(LATR)*COS(DECR)*COS(HORR))
      ALT=ALT*180./PI
      END
