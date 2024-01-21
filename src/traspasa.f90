! **********************************************************************
!                                                    SUBROUTINE TRASPASA
!                                                    *******************
!
      SUBROUTINE TRASPASA
!
! Extrae la informacion de cada linea del fichero con los objetos
! a observar y la descompone en OBJETO,A.R. y Declinacion.
!
! Subrutinas auxiliares: AJUSTA
!                        CAMBIO
!
      IMPLICIT NONE
!---> variables globales: ENTRADA
      CHARACTER*80 LINEA
!---> variables globales: SALIDA
      REAL AR0,DEC0
      CHARACTER*30 NOM0
!---> variables locales
      INTEGER I,IPOS
      REAL RIL
      CHARACTER*30 LAR0,LDEC0,XXX,L1
      CHARACTER*8 LL,L2
!
      COMMON/BLK1/LINEA
      COMMON/BLK2A/NOM0
      COMMON/BLK2B/AR0,DEC0
      COMMON/BLKAJU/L1,L2
      COMMON/BLKCAM/LL,RIL
!
! separamos la linea en tres cadenas (el separador es la coma)
      IPOS=INDEX(LINEA,',')
      IF(IPOS.EQ.0)GOTO 17
      NOM0=LINEA(:IPOS-1)
      LINEA=LINEA(IPOS+1:)
      IPOS=INDEX(LINEA,',')
      IF(IPOS.EQ.0)GOTO 17
      LAR0=LINEA(:IPOS-1)
      LDEC0=LINEA(IPOS+1:)
! eliminamos espacios en blanco al principio de las 3 cadenas
    6 CONTINUE
      IF(NOM0(1:1).EQ.' ')THEN 
        NOM0=NOM0(2:)
        GOTO 6
      END IF
    7 CONTINUE
      IF(LAR0(1:1).EQ.' ')THEN 
        LAR0=LAR0(2:)
        GOTO 7
      END IF
    8 CONTINUE
      IF(LDEC0(1:1).EQ.' ')THEN 
        LDEC0=LDEC0(2:)
        GOTO 8
      END IF
! eliminamos posibles espacios en blanco entre el signo (si existe) y el
! dato numerico
      IF((LAR0(1:1).EQ.'+').OR.(LAR0(1:1).EQ.'-'))THEN
    9   CONTINUE
        IF(LAR0(2:2).EQ.' ')THEN
          XXX=LAR0(1:1)//LAR0(3:)
          LAR0=XXX
          GOTO 9
        END IF
      END IF
      IF((LDEC0(1:1).EQ.'+').OR.(LDEC0(1:1).EQ.'-'))THEN
   10   CONTINUE
        IF(LDEC0(2:2).EQ.' ')THEN
          XXX=LDEC0(1:1)//LDEC0(3:)
          LDEC0=XXX
          GOTO 10
        END IF
      END IF
! Transformaremos las cadenas LAR0 y LDEC0 a variables REAL*4, mediante
! la utilizacion de los valores ASCII de cada caracter en las cadenas.
! Para no perder precision primero haremos la transformacion a un numero
! INTEGER*4 y luego lo convertimos en REAL
!
! ponemos ceros donde no hay un numero, un punto decimal o un signo
      DO I=1,30
        L1(I:I)='0'
        IF((ICHAR(LAR0(I:I)).GE.48).AND.(ICHAR(LAR0(I:I)).LE.57))THEN
          L1(I:I)=LAR0(I:I)
        END IF
        IF(LAR0(I:I).EQ.'.')L1(I:I)=LAR0(I:I)
        IF((LAR0(I:I).EQ.'+').OR.(LAR0(I:I).EQ.'-'))L1(I:I)=LAR0(I:I)
      END DO
      CALL AJUSTA
      LL=L2
      CALL CAMBIO
      AR0=RIL
!
      DO I=1,30
        L1(I:I)='0'
        IF((ICHAR(LDEC0(I:I)).GE.48).AND.(ICHAR(LDEC0(I:I)).LE.57))THEN
          L1(I:I)=LDEC0(I:I)
        END IF
        IF(LDEC0(I:I).EQ.'.')L1(I:I)=LDEC0(I:I)
        IF((LDEC0(I:I).EQ.'+').OR.(LDEC0(I:I).EQ.'-'))L1(I:I)=LDEC0(I:I)
      END DO
      CALL AJUSTA
      LL=L2
      CALL CAMBIO
      DEC0=RIL
!
      RETURN
   17 CONTINUE
      WRITE(*,'(1X,A)')'ERROR: el fichero de objetos a observar tiene'
      WRITE(*,'(1X,A)')'un formato erroneo (subroutine: TRASPASA)'
      STOP
      END
!
! **********************************************************************
!                                                      SUBROUTINE AJUSTA
!                                                      *****************
      SUBROUTINE AJUSTA
!
! Subrutina auxiliar de la subrutina TRASPASA
!
! Toma una variable CHARACTER*30 y la recorta a una variable
! CHARACTER*8 con el formato +DD.MMSS
!
      IMPLICIT NONE
!---> variables locales
      INTEGER IPOS
      CHARACTER*30 L1
      CHARACTER*8 L2
!
      COMMON/BLKAJU/L1,L2
!
      IPOS=INDEX(L1,'.')
      IF((IPOS.EQ.0).OR.(IPOS.GT.4))GOTO 3
      IF(IPOS.EQ.1)L2='+00'//L1(1:5)
      IF(IPOS.EQ.2)THEN
        IF((L1(1:1).EQ.'+').OR.(L1(1:1).EQ.'-'))THEN
          L2=L1(1:1)//'00'//L1(2:6)
        ELSE
          L2='+0'//L1(1:6)
        END IF
      END IF
      IF(IPOS.EQ.3)THEN
        IF((L1(1:1).EQ.'+').OR.(L1(1:1).EQ.'-'))THEN
          L2=L1(1:1)//'0'//L1(2:7)
        ELSE
          L2='+'//L1(1:7)
        END IF
      END IF
      IF(IPOS.EQ.4)L2=L1(1:8)
      RETURN
    3 CONTINUE
      WRITE(*,'(1X,A)')'ERROR:formato del fichero de datos erroneo'
      STOP '(subroutine: AJUSTA)'
      END
!
! **********************************************************************
!                                                      SUBROUTINE CAMBIO
!                                                      *****************
      SUBROUTINE CAMBIO
!
! Subrutina auxiliar de la subrutina TRASPASA
!
! Transforma una cadena de 8 caracteres a un numero real. El formato
! de la cadena de caracteres es +DD.MMSS, igual que el del numero REAL.
! Para no perder precision primero hacemos la transformacion a un
! INTEGER*4 y luego lo convertimos en REAL.
!
      IMPLICIT NONE
!---> variables locales
      INTEGER I
      INTEGER*4 IL
      REAL RIL
      CHARACTER*8 LL
!
      COMMON/BLKCAM/LL,RIL
!
      IL=0
      DO I=2,3
        IL=IL+(ICHAR(LL(I:I))-48)*10**(8-I)
      END DO
      DO I=5,8
        IL=IL+(ICHAR(LL(I:I))-48)*10**(9-I)
      END DO
      IF(LL(1:1).EQ.'-')IL=-IL
      RIL=REAL(IL)/100000
      END
