      SUBROUTINE DIBUOBJE(OPCMEN)
!
      IMPLICIT NONE
      CHARACTER*1 OPCMEN
!
      INTEGER NPTOS
      PARAMETER(NPTOS=100)
      REAL PI
      PARAMETER(PI=3.141593)
      INTEGER NPTOSF
      INTEGER IALEAT,NBODY
!     REAL TUINI,TUCREPV,TUCREPM,TUFIN
      REAL TUINI_,TUFIN_
      REAL TU(NPTOS),ACIMUT(NPTOS),ALTURA(NPTOS)
!
      REAL XMIN,XMAX,YMIN,YMAX
      REAL XCEN,YCEN,RADG
      REAL X,Y
      REAL X01,Y01
      REAL X02,Y02
      REAL RADIO
      LOGICAL GRAF
!
      INTEGER J
      INTEGER NB
      CHARACTER*10 NUMERO
!
      COMMON/BLKAAN/TU,ACIMUT,ALTURA,NPTOSF
      COMMON/BALEAT/IALEAT,NBODY
!     COMMON/BLKTU/TUINI,TUCREPV,TUCREPM,TUFIN
      COMMON/BLKTU_/TUINI_,TUFIN_
      COMMON/BLKBOX/XMIN,XMAX,YMIN,YMAX
      COMMON/BLKCIR/XCEN,YCEN,RADG
!
! fijamos un maximo de 10 indicaciones de grafica por objeto
!      NB=NPTOS/10
      NB=10
!
      GRAF=.FALSE.
      DO J=1,NPTOSF
        IF(ALTURA(J).GE.0)THEN
          Y=YMIN+(YMAX-YMIN)*ALTURA(J)/90
          X=XMIN+(XMAX-XMIN)*(TU(J)-TUINI_)/(TUFIN_-TUINI_)
          IF(GRAF)THEN
            CALL PLMOVE(X01,Y01)
            CALL PLDRAW(X,Y)
          ELSE
            CALL PLPOINT(1,X,Y,-1)
          END IF
          X01=X
          Y01=Y
          IF(OPCMEN.NE.'5')THEN
            IF(REAL(J+IALEAT)/NB.EQ.REAL((J+IALEAT)/NB))THEN
              CALL PLSCH(0.4)
              WRITE(NUMERO,'(I2.2)')NBODY
!cc              CALL PLPTEXT(X,Y+.5,0.,.5,NUMERO(2-NBODY/10:2))
              CALL PLPTEXT(X,Y+.5,0.,.5,NUMERO)
            END IF
          END IF
!
          IF(OPCMEN.EQ.'1')THEN
            RADIO=RADG*(90-ALTURA(J))/90
            X=XCEN-RADIO*SIN(ACIMUT(J)*PI/180)
            Y=YCEN-RADIO*COS(ACIMUT(J)*PI/180)
            IF(GRAF)THEN
              CALL PLMOVE(X02,Y02)
              CALL PLDRAW(X,Y)
            ELSE
              CALL PLPOINT(1,X,Y,-1)
            END IF
            X02=X
            Y02=Y
            IF(REAL(J+IALEAT)/NB.EQ.REAL((J+IALEAT)/NB))THEN
              CALL PLSCH(0.3)
              WRITE(NUMERO,'(I2.2)')NBODY
              CALL PLPTEXT(X,Y+.5,0.,.5,NUMERO(2-NBODY/10:2))
            END IF
          END IF
          GRAF=.TRUE.
        ELSE
          GRAF=.FALSE.
        END IF
      END DO
      END
