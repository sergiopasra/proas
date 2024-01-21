!
! **********************************************************************
!
!            PROAS: PROGRAMACION DE OBSERVACIONES ASTRONOMICAS
!
!             Versión inicial: 17-diciembre-1997  (N. Cardiel)
!
! **********************************************************************

PROGRAM PROAS
!
        use :: plplot
      IMPLICIT NONE
      INCLUDE 'proas.inc'
      INCLUDE 'version.inc'
!
!-----------------------------------------------------------------------
! parametros
      INTEGER NOBJE,NPTOS
      PARAMETER(NOBJE=1000,NPTOS=100)
!
! NOBJE=numero maximo de objetos a observar
! NPTOS=numero maximo de puntos a dibujar en las graficas
!
      REAL PI
      PARAMETER(PI=3.141593)
!-----------------------------------------------------------------------
! numero de puntos que con representamos lineas
      INTEGER NPTOSF
! contadores genericos en bucles
      INTEGER I,K,KK
! numero de objetos leidos
      INTEGER MT
! entero aleatorio entre 1 y 20
      INTEGER IALEAT
! numero de objetos por carta
      INTEGER NOBJEG
! numero de objeto dibujado en una misma grafica
      INTEGER NBODY
!-----------------------------------------------------------------------
! semilla para el generador de numeros aleatorios
      INTEGER*4 NSEED
! funcion externa en C para generar numeros aleatorios
      EXTERNAL RANRED
      REAL RANRED
!-----------------------------------------------------------------------
! latitud, longitud y altura del observatorio
      REAL LAT,LONG,LAT0,LONG0,ALTOBS
      REAL ALH,ALM,ALS
! fecha
      INTEGER IANO,IMES,IDIA
      REAL ANO,MES,MONTH,DIA,HORA
! elementos precesionales
      REAL GIO,ZETA,TETA
! matriz de coeficientes para transformacion precesional
      REAL M(3,3)
! ascension recta y declinacion de los objetos
      REAL AR(NOBJE),DEC(NOBJE)
      REAL ARINI(NOBJE),DECINI(NOBJE)
      REAL AR0,DEC0
! ascension recta y declinacion del Sol
      REAL ARSUN,DECSUN
! coordenadas rectangulares
      REAL X(3),X0(3) 
! fechas iniciales y finales de equinoccio de las coordenadas
      REAL TII,TFF,TI,TF
! tiempo sidereo en Greenwich a 0h T.U.
      REAL TS0
!cc      REAL ATS0H,ATS0M,ATS0S
! angulos horarios y azimuts de ortos y ocasos
      REAL HORTO,HOCASO
      REAL AORTO,AOCASO
! altura de culminaciones
      REAL ALTCUL
! radio terrestre para el observatorio
      REAL REARTH
! semiejes del elipsoide WGS-84
      REAL SEMIA,SEMIB
! angulo de depresion del horizonte debido a la altura
      REAL ANGHOA
! distancia cenital
      REAL DISZEN
! tiempo universal de ortos y ocasos
      REAL TUORTO1,TUORTO2
      REAL TUOCASO1,TUOCASO2
! tiempo universal de fin y principio de crepusculos
      REAL TUCREPV1,TUCREPV2
      REAL TUCREPM1,TUCREPM2
      REAL TUCREPV,TUCREPM
! duracion de la noche cerrada
      REAL DNOCHE
! tiempo universal para calculo de altura de los objetos
      REAL TUINI,TUFIN,TU(NPTOS)
! numero de horas antes puesta sol/despues salida del sol, para diagramas
      REAL DELTAT
! valores de TUINI y TUFIN corregidos de DELTAT
      REAL TUINI_,TUFIN_
! tiempo universal de la culminacion de los objetos
      REAL TUCULM
! tiempo sidereo local y tiempo sidereo en Greenwich
      REAL TSL,TSG
! angulo horario de los objetos para un instante TU determinado
      REAL HOR
! coordenadas horizontales de los objetos
      REAL ACIMUT(NPTOS),ALTURA(NPTOS)
! numero aleatorio entre 0.0 (inclusive) y 1.0 (exclusive)
      REAL RALEAT
! puntero para la posicion de los datos de cada objeto
      REAL YPGP
!-----------------------------------------------------------------------
! fecha juliana
      DOUBLE PRECISION FJ
!-----------------------------------------------------------------------
! numero de opcion de entrada de datos
      CHARACTER*1 NOPC
! opcion en el menu principal del programa
      CHARACTER*1 OPCMEN
! opcion salida grafica
      CHARACTER*1 OPCOUT
! opcion de grabar fichero con objetos y posiciones
      CHARACTER*1 OPC
! signo
      CHARACTER*1 SIGNO
! estado de visibilidad de un objeto
      CHARACTER*1 ESTADO
! direccion de culminaciones
      CHARACTER*1 DONCUL
! nombre de los meses
      CHARACTER*10 MESNAME(12)
! dispositivo salida grafica
      CHARACTER*10 SALIDAG
! nombre de los observatorios
      CHARACTER*15 OBS
! nombre de los objetos a observar
      CHARACTER*30 NOM(NOBJE),NOM0
! nombre del fichero con los datos de los objetos y linea del fichero
      CHARACTER*80 FILNAM,LINEA
!-----------------------------------------------------------------------
! variable logica que determina la existencia del fichero de datos
      LOGICAL FILEX
! variables logicas para determinar si existe fichero de control
      LOGICAL LRUN,LMANUAL
! variable temporal para bucles
      LOGICAL LOOP
!-----------------------------------------------------------------------
      COMMON/BLK1/LINEA
      COMMON/BLK2A/NOM0
      COMMON/BLK2B/AR0,DEC0
      COMMON/BLKFJ1/ANO,MES,DIA,HORA
      COMMON/BLKFJ2/FJ
      COMMON/BLKFJ3/TS0
      COMMON/BPOSOL/ARSUN,DECSUN
      COMMON/BLKOBS/LAT,LONG
      COMMON/BLKOOC1/ESTADO,DONCUL
      COMMON/BLKOOC2/HORTO,HOCASO,AORTO,AOCASO,ALTCUL
      COMMON/BLKTU/TUINI,TUCREPV,TUCREPM,TUFIN
      COMMON/BLKTU_/TUINI_,TUFIN_
      COMMON/BALEAT/IALEAT,NBODY
      COMMON/BLKAAN/TU,ACIMUT,ALTURA,NPTOSF
!-----------------------------------------------------------------------
      NOPC='1'
      OPCMEN='1'
      OPCOUT='1'
!
      DATA MESNAME/'JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY','AUGUST','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER'/
!
      WRITE(*,*)
      WRITE(*,104)
      WRITE(*,101)'                                  '&
      &'Proas: planning of astronomical observations'
      WRITE(*,101)'                          '& 
      &'   Initial Version 18-December-1997, Nicolas Cardiel'
      WRITE(*,101)'                                  '& 
      &'         Departamento de Astrofisica, U.C.M.'
      WRITE(*,100) 'Current version: '
      WRITE(*,*) VERSION
!                                                 **********************
!                                                 ESCOGEMOS OBSERVATORIO
!                                                 **********************
      CALL OBSERVAT(OBS,LAT0,LONG0,ALTOBS)
      CALL DECIMAL(LAT0,LAT)
      CALL DECIMAL(LONG0,LONG)
      LONG=LONG/15.
!                                                   ********************
!                                                   INTRODUCIMOS OBJETOS
!                                                   ********************
   10 CONTINUE
      WRITE(*,104)
      WRITE(*,101)'COORDINATES OF THE TARGET LIST:'
      WRITE(*,*)
      WRITE(*,101)'(1) enter object data through keyboard'
      WRITE(*,101)'(2) enter object data from file'
      WRITE(*,*)
      WRITE(*,100)'Option '
      NOPC=READC(NOPC,'12')
      IF(NOPC.EQ.'2')THEN
        WRITE(*,100)'Object data file name'
        FILNAM=READC('@','@')
        INQUIRE(FILE=FILNAM,EXIST=FILEX)
        IF(FILEX)THEN
        ELSE
          WRITE(*,101)'ERROR: this file does not exist.'
          WRITE(*,100)'Press <CR> to continue...'
          READ(*,*)
          CALL LRUNX(LRUN,LMANUAL)
          IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
          GOTO 10
        END IF
      END IF
!
      MT=0
      IF(NOPC.EQ.'1')THEN
   20   CONTINUE
        WRITE(*,100)'Object name (<end> to exit, max. 30 characters) '
        NOM0=READC('end','@')
        CALL CHLOWER(NOM0)
        IF(NOM0.EQ.'end') GOTO 22
        MT=MT+1
        NOM(MT)=NOM0
        WRITE(*,100)'Right ascension (HH.MMSS)'
        ARINI(MT)=READF('@')
        WRITE(*,100)'Declination    (+DD.MMSS)'
        DECINI(MT)=READF('@')
        GOTO 20
      ELSE
        OPEN(UNIT=33,FILE=FILNAM,STATUS='OLD',ERR=990)
   21   CONTINUE
        READ(33,'(A)',END=25)LINEA
        CALL TRASPASA
        MT=MT+1
        NOM(MT)=NOM0
        CALL DECIMAL(AR0,ARINI(MT))
        CALL DECIMAL(DEC0,DECINI(MT))
        WRITE(*,170)NOM(MT),AR0,DEC0
        GOTO 21
      END IF
   22 CONTINUE
      IF(MT.EQ.0)THEN
        WRITE(*,*)
        WRITE(*,101)'ERROR: no. of objects = 0'
        WRITE(*,100)'Press <CR> to continue...'
        READ(*,*)
        CALL LRUNX(LRUN,LMANUAL)
        IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
        GOTO 10
      END IF      
      WRITE(*,100)'Do you want to create an output file with'
      WRITE(*,100)' current object list (y/n) '
      OPC=READC('n','yn')
      IF(OPC.EQ.'y')THEN
        WRITE(*,100)'File name? '
        READ(*,'(A)')FILNAM
        OPEN(UNIT=33,FILE=FILNAM,STATUS='UNKNOWN',ERR=990)
        DO I=1,MT
          WRITE(33,150)NOM(I),ARINI(I),DECINI(I)
        END DO
        CLOSE(UNIT=33)
      END IF
      DO I=1,MT
        CALL DECIMAL(ARINI(I),ARINI(I))
        CALL DECIMAL(DECINI(I),DECINI(I))
      END DO
      GOTO 30
   25 CONTINUE
      CLOSE(UNIT=33)
   30 CONTINUE
      IF(MT.EQ.0)THEN
        WRITE(*,*)
        WRITE(*,101)'ERROR: no. of objects = 0'
        WRITE(*,100)'Press <CR> to continue...'
        READ(*,*)
        CALL LRUNX(LRUN,LMANUAL)
        IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
        GOTO 10
      END IF
      WRITE(*,*)
      WRITE(*,'(A,I3)')'Total number of objets: ',MT
      WRITE(*,*)
      WRITE(*,104)
      WRITE(*,100)'Equinox (for all the objects) '
      TII=READF('2000.0')
!                                                   ********************
!                                                   FECHA DE OBSERVACION
!                                                   ********************
      WRITE(*,104)
   40 CONTINUE
      WRITE(*,101)'ENTER OBSERVING DATE:'
      WRITE(*,*)
      WRITE(*,100)'YEAR          '
      IANO=READI('@')
      ANO=REAL(IANO)
      WRITE(*,100)'MONTH (number)' 
      IMES=READI('@')
      IF((IMES.LT.1).OR.(IMES.GT.12))THEN
        WRITE(*,101)'ERROR: invalid month number.'
        WRITE(*,100)'Press <CR> to continue...'
        READ(*,*)
        CALL LRUNX(LRUN,LMANUAL)
        IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
        GOTO 40
      END IF
      MES=REAL(IMES)
      WRITE(*,100)'DAY           '
      IDIA=READI('@')
      DIA=REAL(IDIA)
!                                                              *********
!                                                              PRECESION
!                                                              *********
      IF(INT(MES).LE.2)THEN
        MONTH=AINT((MES-1)*63./2.)
      ELSE
        MONTH=AINT((MES+1)*30.6)-63.
      END IF
      MONTH=MONTH+DIA
      TFF=ANO+MONTH/365.25
      TI=(TII-2000.)/100.
      TF=(TFF-2000.-100.*TI)/100.
! elementos precesionales en grados
      GIO=((2306.2181+1.39656*TI-.000139*TI*TI)*TF+(.30188-.000344*TI)&
        &*TF*TF+.017998*TF*TF*TF)/3600.
      ZETA=GIO+((.7928+.00041*TI)*TF*TF+.000205*TF*TF*TF)/3600.
      TETA=((2004.3109-.8533*TI-.000217*TI*TI)*TF-(.42665+.000217*TI)&
         &*TF*TF-.041833*TF*TF*TF)/3600.
! los pasamos a radianes
      GIO=GIO*PI/180.
      ZETA=ZETA*PI/180.
      TETA=TETA*PI/180.
! matriz de rotacion
      M(1,1)=-(SIN(GIO)*SIN(ZETA))+COS(GIO)*COS(TETA)*COS(ZETA)
      M(1,2)=-(COS(GIO)*SIN(ZETA))-(SIN(GIO)*COS(ZETA)*COS(TETA))
      M(1,3)=-(SIN(TETA)*COS(ZETA))
      M(2,1)=SIN(GIO)*COS(ZETA)+COS(GIO)*COS(TETA)*SIN(ZETA)
      M(2,2)=COS(GIO)*COS(ZETA)-SIN(GIO)*COS(TETA)*SIN(ZETA)
      M(2,3)=-(SIN(TETA)*SIN(ZETA))
      M(3,1)=COS(GIO)*SIN(TETA)
      M(3,2)=-(SIN(GIO)*SIN(TETA))
      M(3,3)=COS(TETA)
! coordenadas rectangulares del objeto
      DO 410,I=1,MT
        X0(1)=COS(DECINI(I)*PI/180.)*COS(ARINI(I)*15.*PI/180.)
        X0(2)=COS(DECINI(I)*PI/180.)*SIN(ARINI(I)*15.*PI/180.)
        X0(3)=SIN(DECINI(I)*PI/180.)
! cambio a coordenadas de la epoca
        DO 405,K=1,3
          X(K)=0.
          DO 400,KK=1,3
            X(K)=X(K)+X0(KK)*M(K,KK)
  400     CONTINUE
  405   CONTINUE
        AR(I)=ATAN2(X(2),X(1))
        DEC(I)=ASIN(X(3))
        AR(I)=AR(I)*180./PI/15.
        IF(AR(I).LT.0.)AR(I)=AR(I)+24.
        DEC(I)=DEC(I)*180./PI
  410 CONTINUE
!                                                   ********************
!                                                   FECHA JULIANA Y TSG0
!                                                   ********************
! Calculamos la Fecha Juliana(FJ) y el TS a 0h TU en Greenwich(TS0)
      HORA=0.
      CALL FJ_TS0
!                                         ******************************
!                                         POSICION DEL SOL, ORTO Y OCASO
!                                         ******************************
! Iteramos para calcular la posicion del Sol en el instante del ocaso y
! del orto. Tomamos como valor inicial la posicion a 0h T.U. del dia
! de la observacion.
      CALL POSSOL(0)
! DISZEN: refraccion 34'
!         semidiametro del Sol: 16'
!         depresion del horizonte: usamos elipsoide WGS-84
      SEMIA=6378.137
      SEMIB=6356.752
      REARTH=1/SQRT(COS(LAT*PI/180.)*COS(LAT*PI/180.)/SEMIA/SEMIA+&
            SIN(LAT*PI/180.)*SIN(LAT*PI/180.)/SEMIB/SEMIB)
      ANGHOA=ACOS(REARTH/(REARTH+ALTOBS/1000.))
      ANGHOA=ANGHOA*180./PI
      DISZEN=90.+34./60.+16./60.+ANGHOA
      CALL ORCASO(DECSUN,DISZEN)
      IF(ESTADO.EQ.'C')THEN
        WRITE(*,101)'Sol temporalmente circumpolar sin ortos ni ocasos'
        STOP
      ELSE IF(ESTADO.EQ.'I')THEN
        WRITE(*,101)'Sol temporalmente invisible sin ortos ni ocasos'
        STOP
      END IF
! ocaso del Sol (calculamos el ocaso que tiene lugar a partir de las 0h
! de T.U. del dia de la observacion).
      CALL TIEMPOU(ARSUN,HOCASO,LONG,TS0,TUOCASO1)
  200 CONTINUE
      HORA=TUOCASO1
      CALL FJ_TS0
      CALL POSSOL(0)
      CALL ORCASO(DECSUN,DISZEN)
      CALL TIEMPOU(ARSUN,HOCASO,LONG,TS0,TUOCASO2)
!cc     type*,'tuocaso1,2: ',tuocaso1,tuocaso2
      IF(ABS(TUOCASO1-TUOCASO2).GT.12.0)THEN
        IF(TUOCASO1.GT.TUOCASO2)THEN
          TUOCASO2=TUOCASO2+24.
        ELSE
          TUOCASO1=TUOCASO1+24.
        END IF
      END IF
      IF(ABS(TUOCASO1-TUOCASO2).GT.0.0003)THEN
        TUOCASO1=TUOCASO2
        GOTO 200
      END IF
! crepusculo astronomico vespertino (utilizamos como valor inicial en
! la iteracion el instante del ocaso)
      TUCREPV1=TUOCASO2
  201 CONTINUE
      HORA=TUCREPV1
      CALL FJ_TS0
      CALL POSSOL(0)
      CALL ORCASO(DECSUN,108.0+ANGHOA)
      IF(ESTADO.EQ.'C')THEN
        WRITE(*,101)'Crepusculo toda la noche'
        STOP
      END IF
      CALL TIEMPOU(ARSUN,HOCASO,LONG,TS0,TUCREPV2)
!cc     type*,'tucrepv1,2: ',tucrepv1,tucrepv2
      IF(ABS(TUCREPV1-TUCREPV2).GT.12.0)THEN
        IF(TUCREPV1.GT.TUCREPV2)THEN
          TUCREPV2=TUCREPV2+24.
        ELSE
          TUCREPV1=TUCREPV1+24.
        END IF
      END IF
      IF(ABS(TUCREPV2-TUCREPV1).GT.0.0003)THEN
        TUCREPV1=TUCREPV2
        GOTO 201
      END IF
! orto del Sol (calculamos el orto que tiene lugar a partir de las 0h
! de T.U. del dia de la observacion)
      TUORTO1=TUOCASO2
  210 CONTINUE
      HORA=TUORTO1
      CALL FJ_TS0
      CALL POSSOL(0)
      CALL ORCASO(DECSUN,DISZEN)
      CALL TIEMPOU(ARSUN,HORTO,LONG,TS0,TUORTO2)
!cc     type*,'tuorto1,2: ',tuorto1,tuorto2
      IF(ABS(TUORTO1-TUORTO2).GT.12.0)THEN
        IF(TUORTO1.GT.TUORTO2)THEN
          TUORTO2=TUORTO2+24.
        ELSE
          TUORTO1=TUORTO1+24.
        END IF
      END IF
      IF(ABS(TUORTO1-TUORTO2).GT.0.0003)THEN
        TUORTO1=TUORTO2
        GOTO 210
      END IF
! crepusculo astronomico matutino (utilizamos como valor inicial en
! la iteracion el instante del orto)
      TUCREPM1=TUORTO2
  211 CONTINUE
      HORA=TUCREPM1
      CALL FJ_TS0
      CALL POSSOL(0)
      CALL ORCASO(DECSUN,108.0+ANGHOA)
      CALL TIEMPOU(ARSUN,HORTO,LONG,TS0,TUCREPM2)
!cc     type*,'tucrepm1,2: ',tucrepm1,tucrepm2
      IF(ABS(TUCREPM1-TUCREPM2).GT.12.0)THEN
        IF(TUCREPM1.GT.TUCREPM2)THEN
          TUCREPM2=TUCREPM2+24.
        ELSE
          TUCREPM1=TUCREPM1+24.
        END IF
      END IF
      IF(ABS(TUCREPM2-TUCREPM1).GT.0.0003)THEN
        TUCREPM1=TUCREPM2
        GOTO 211
      END IF
! si el orto calculado es anterior al ocaso quiere decir que tenemos
! que recalcular el orto que ocurre para el dia siguiente al dia
! de la observacion.
      IF(TUORTO2.LT.TUOCASO2)THEN
        TUORTO1=TUORTO2
  220   CONTINUE
        HORA=TUORTO1+24
        CALL FJ_TS0
! TS 0h T.U. del dia siguiente al de la observacion
        TS0=TS0+24.*2.737909E-3
        IF(TS0.GT.24.)TS0=TS0-24.
! posicion del Sol para el dia siguiente al de la observacion
        CALL POSSOL(0)
        CALL ORCASO(DECSUN,DISZEN)
        CALL TIEMPOU(ARSUN,HORTO,LONG,TS0,TUORTO2)
!cc     type*,'tuorto1,2: ',tuorto1,tuorto2

        IF(ABS(TUORTO1-TUORTO2).GT.12.0)THEN
           IF(TUORTO1.GT.TUORTO2)THEN
             TUORTO2=TUORTO2+24.
           ELSE
             TUORTO1=TUORTO1+24.
           END IF
        END IF


        IF(ABS(TUORTO1-TUORTO2).GT.0.0003)THEN
          TUORTO1=TUORTO2
          GOTO 220
        END IF
! crepusculo astronomico matutino (utilizamos como valor inicial en
! la iteracion el instante del crepusculo para el dia anterior)
        TUCREPM1=TUCREPM2
  221   CONTINUE
        HORA=TUCREPM1+24
        CALL FJ_TS0
! TS 0h T.U. del dia siguiente al de la observacion
        TS0=TS0+24.*2.737909E-3
        IF(TS0.GT.24.)TS0=TS0-24.
! posicion del Sol para el dia siguiente al de la observacion
        CALL POSSOL(0)
        CALL ORCASO(DECSUN,108.0+ANGHOA)
        CALL TIEMPOU(ARSUN,HORTO,LONG,TS0,TUCREPM2)
!cc     type*,'tucrepm1,2: ',tucrepm1,tucrepm2
        IF(ABS(TUCREPM2-TUCREPM1).GT.0.0003)THEN
          TUCREPM1=TUCREPM2
          GOTO 221
        END IF
      END IF
!
! limites temporales en los cuales trabajaremos
      TUINI=TUOCASO2
      TUCREPV=TUCREPV2
      TUCREPM=TUCREPM2
      TUFIN=TUORTO2
!cc     type*,TUINI,TUCREPV,TUCREPM,TUFIN
      IF(TUCREPV.LT.TUINI) TUCREPV=TUCREPV+24
      IF(TUCREPM.LT.TUCREPV) TUCREPM=TUCREPM+24
      IF(TUFIN.LT.TUCREPM) TUFIN=TUFIN+24
      LOOP=.TRUE.
      DO WHILE(LOOP)
        WRITE(*,100)'Expansion time (hours) before sunset/after '&
        &'sunrise '
        DELTAT=READF('0.0')
        IF(DELTAT.GE.0.0) LOOP=.FALSE.
        IF(LOOP)THEN
          WRITE(*,101) 'ERROR: this number must be >= 0.0'
          WRITE(*,*)
        END IF
      END DO
      TUINI_=TUINI-DELTAT
      TUFIN_=TUFIN+DELTAT
!cc     type*,TUINI,TUCREPV,TUCREPM,TUFIN
! recalculamos la FJ y TS0 para el dia de la observacion a 0h TU
      HORA=0.
      CALL FJ_TS0
!                                                       ****************
!                                                       MENU DE OPCIONES
!                                                       ****************
  300 CONTINUE
      WRITE(*,104)
      WRITE(*,*)
      WRITE(*,101)'(1) plot composite chart'
      WRITE(*,101)'(2) plot only altitude vs UT from option (1)'
      WRITE(*,101)'(3) plot altitude vs UT (draft)'
      WRITE(*,101)'(4) change observing date'
      WRITE(*,101)'(0) STOP'
      WRITE(*,*)
      WRITE(*,100)'Option '
      OPCMEN=READC(OPCMEN,'01234')
      IF(OPCMEN.EQ.'0')GOTO 560
      IF(OPCMEN.EQ.'4') GOTO 40
      IF(OPCMEN.EQ.'1')THEN
        WRITE(*,100)'No. of objets/plot '
        NOBJEG=READILIM('10',1,10)
        NPTOSF=100
      ELSE IF(OPCMEN.EQ.'3')THEN
        WRITE(*,100)'No. of objets/plot '
        NOBJEG=READILIM('40',1,40)
!cc     IF(NOBJEG.GT.40) NOBJEG=40
!cc     WRITE(*,100)'Num. puntos que definen la trayectoria '//
!cc     +   'de los objetos '
!cc     NPTOSF=READILIM('50',1,100)
        NPTOSF=100
      ELSE
        WRITE(*,100)'No. of objets/plot '
        NOBJEG=READI('@')
!cc     NPTOSF=50
        NPTOSF=100
!cc        OPEN(UNIT=33,FILE='proas.dat',STATUS='UNKNOWN')
      END IF
      IF(NOBJEG.LT.1)NOBJEG=1
!
  310 CONTINUE
      WRITE(*,104)
      WRITE(*,101)'GRAPHIC OUTPUT: '
      WRITE(*,*)
      WRITE(*,101)'(1) /xserve (terminal)'
      WRITE(*,101)'(2) /ps (PostScript)'
      WRITE(*,100)'(3) show list with available graphic output'
      WRITE(*,100)' devices'
      WRITE(*,*)
      WRITE(*,100)'Option '
      OPCOUT=READC(OPCOUT,'123')
      WRITE(*,*)
! la semilla de los numeros aleatorios es -1 para inicializar el generador en
! la primera llamada
      NSEED=-1
!
      IF(OPCOUT.EQ.'1')THEN
        SALIDAG='/xserve'
      ELSE IF(OPCOUT.EQ.'2')THEN
        SALIDAG='/ps'
      ELSE IF(OPCOUT.EQ.'3')THEN
        SALIDAG='?'
      END IF
!                                        *******************************
!                                        DIBUJAMOS CARTA CON LOS OBJETOS
!                                        *******************************
      CALL PLBEGIN(0,SALIDAG,1,1)
      CALL PLASK(.FALSE.)
      DO 550,I=1,MT
        IF(REAL(I-1)/NOBJEG.EQ.REAL((I-1)/NOBJEG))THEN
          IF(I.NE.1)THEN
            IF(OPCOUT.NE.'2')THEN
              WRITE(*,100)'Press <CR> to continue...'
              READ(*,*)
              CALL LRUNX(LRUN,LMANUAL)
              IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
            END IF
            CALL PLPAGE
          END IF
          NBODY=0.
          CALL DIBUHOJA(OPCMEN)
!***>
            CALL PLSCH(0.7)
! observatorio
            WRITE(LINEA,'(A29)')'Observatory: '//OBS
          IF(OPCMEN.EQ.'1')THEN
            CALL PLPTEXT(200.,72.,0.,0.,LINEA(1:29))
          ELSE IF(OPCMEN.EQ.'2')THEN
            CALL PLPTEXT(80.,185.,0.,0.,LINEA(1:29))
!cc         WRITE(33,104)
!cc         WRITE(33,101)LINEA(1:29)
          ELSE
            CALL PLPTEXT(50.,185.,0.,0.,LINEA(1:29))
          END IF
          IF(OPCMEN.EQ.'1')THEN
! latitud
            CALL PLPTEXT(239.,68.,0.,1.,'Latitude:')
            WRITE(LINEA,'(F8.4)')LAT0
            IF(LINEA(1:1).EQ.' ') LINEA(1:1)='+'
            LINEA(1:18)=LINEA(1:3)//'\\uo\\d '//LINEA(5:6)//CHAR(39)//' '//LINEA(7:8)//'"'
            CALL PLPTEXT(240.,68.,0.,0.,' '//LINEA(1:18))
! longitud
            CALL PLPTEXT(239.,64.,0.,1.,'Longitude:')
            WRITE(LINEA,'(F9.4)')LONG0
            LINEA(1:18)=LINEA(1:4)//'\\uo\\d '//LINEA(6:7)//CHAR(39)//' '//LINEA(8:9)//'"'
            CALL PLPTEXT(240.,64.,0.,0.,LINEA(1:18))
! altura
            CALL PLPTEXT(239.,60.,0.,1.,'Height:')
            WRITE(LINEA,'(F5.0,A2)')ALTOBS,' m'
            CALL PLPTEXT(240.,60.,0.,0.,LINEA(1:7))
          END IF
! fecha
            WRITE(LINEA,'(A7,I4.4,A2,I2.2,A11)')'DATE: ',INT(ANO),', ',INT(DIA),' '//MESNAME(INT(MES))
          IF(OPCMEN.EQ.'1')THEN
            CALL PLPTEXT(200.,52.,0.,0.,LINEA(1:29))
          ELSE IF(OPCMEN.EQ.'2')THEN
            CALL PLPTEXT(150.,185.,0.,0.,LINEA(1:29))
!cc         WRITE(33,101)LINEA(1:29)
          ELSE
            CALL PLPTEXT(120.,185.,0.,0.,LINEA(1:29))
          END IF
          IF(OPCMEN.EQ.'1')THEN
! fecha juliana
            CALL PLPTEXT(239.,48.,0.,1.,'JD (at 0h UT):')
            WRITE(LINEA,'(F9.1)')FJ
            CALL PLPTEXT(240.,48.,0.,0.,LINEA(1:9))
! TSG(0h TU)
            CALL PLPTEXT(239.,44.,0.,1.,'GST (at 0h UT):')
            CALL SEXAG(TS0,ALH,ALM,ALS)
            WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),'m ',INT(ALS),'s'
            CALL PLPTEXT(240.,44.,0.,0.,LINEA(1:11))
! TSL(0h TU)
            TSL=TS0+LONG
            IF(TSL.GT.24)TSL=TSL-24.
            IF(TSL.LE.0.)TSL=TSL+24.
            CALL SEXAG(TSL,ALH,ALM,ALS)
            CALL PLPTEXT(239.,40.,0.,1.,'LST (at 0h UT):')
            WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),'m ',INT(ALS),'s'
            CALL PLPTEXT(240.,40.,0.,0.,LINEA(1:11))
! ocaso del Sol
            CALL PLPTEXT(239.,32.,0.,1.,'sunset:')
            CALL SEXAG(TUINI,ALH,ALM,ALS)
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),'h ',INT(ALM),'m'
            CALL PLPTEXT(240.,32.,0.,0.,LINEA(1:7)//' (UT)')
! fin crepusculo astronomico vespertino
            CALL PLPTEXT(239.,28.,0.,1.,'end of twilight:')
            IF(TUCREPV.GT.24)THEN
              CALL SEXAG(TUCREPV-24.,ALH,ALM,ALS)
            ELSE
              CALL SEXAG(TUCREPV,ALH,ALM,ALS)
            END IF
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),'h ',INT(ALM),'m'
            CALL PLPTEXT(240.,28.,0.,0.,LINEA(1:7)//' (UT)')
! inicio crepusculo astronomico matutino
            CALL PLPTEXT(239.,24.,0.,1.,'beginning of twilight:')
            IF(TUCREPM.GT.24.)THEN
              CALL SEXAG(TUCREPM-24,ALH,ALM,ALS)
            ELSE
              CALL SEXAG(TUCREPM,ALH,ALM,ALS)
            END IF
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),'h ',INT(ALM),'m'
            CALL PLPTEXT(240.,24.,0.,0.,LINEA(1:7)//' (UT)')
! orto del Sol
            CALL PLPTEXT(239.,20.,0.,1.,'sunrise:')
            IF(TUFIN.GT.24)THEN
              CALL SEXAG(TUFIN-24.,ALH,ALM,ALS)
            ELSE
              CALL SEXAG(TUFIN,ALH,ALM,ALS)
            END IF
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),'h ',INT(ALM),'m'
            CALL PLPTEXT(240.,20.,0.,0.,LINEA(1:7)//' (UT)')
! duracion de la noche cerrada
            DNOCHE=TUCREPM-TUCREPV
            IF(TUCREPM.LT.TUCREPV)DNOCHE=24+DNOCHE
            CALL PLPTEXT(249.,16.,0.,1.,'Night time free of twilight:')
            CALL SEXAG(DNOCHE,ALH,ALM,ALS)
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),'h ',INT(ALM),'m'
            CALL PLPTEXT(250.,16.,0.,0.,LINEA(1:7))
! copywrite
            LINEA='\\(0274) cardiel@fis.ucm.es (UCM)'
            CALL PLSCH(0.45)
            CALL PLPTEXT(279.,1.,0.,1.,LINEA(1:TRUELEN(LINEA)))
!***>
          END IF
        END IF
        RALEAT=RANRED(NSEED)
        IALEAT=INT(RALEAT*20)+1
        NBODY=NBODY+1
        DO 520,K=1,NPTOSF
! instante en T.U. en que calculamos la posicon
          TU(K)=TUINI_+(TUFIN_-TUINI_)*REAL(K-1)/REAL(NPTOSF-1)
! calculamos el TSG para el instante TU
          TSG=TS0+TU(K)+TU(K)*2.737909E-3
! TSL para el instante TU
          TSL=TSG+LONG
! angulo horario
          HOR=TSL-AR(I)
! pasamos a coordenadas horizontales
          CALL CAMBCOOR(HOR,DEC(I),ACIMUT(K),ALTURA(K))
  520   CONTINUE
        CALL DIBUOBJE(OPCMEN)
        IF(OPCMEN.EQ.'1')THEN
!***>
          CALL PLSCH(0.6)
! objeto
          WRITE(LINEA,'(A8,I2,A17)')'OBJECT #',NBODY,': '//NOM(I)
          YPGP=1.5+(10.-REAL(NBODY))*19.+14.
          CALL PLPTEXT(3.,YPGP,0.,0.,LINEA(1:38))
! ascension recta epoca inicial
          YPGP=YPGP-4.0
          CALL PLPTEXT(3.,YPGP,0.,0.,'\\ga:')
          CALL SEXAG(ARINI(I),ALH,ALM,ALS)
          WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),'m ',INT(ALS),'s'
          CALL PLPTEXT(8.,YPGP,0.,0.,LINEA(1:11))
! declinacion epoca inicial
          CALL PLPTEXT(35.,YPGP,0.,0.,'\\gd:')
          CALL SEXAG(DECINI(I),ALH,ALM,ALS)
          SIGNO='+'
          IF(DECINI(I).LT.0)THEN
            SIGNO='-'
            ALH=ABS(ALH)
            ALM=ABS(ALM)
            ALS=ABS(ALS)
          END IF
          WRITE(LINEA,'(A1,I2.2,A,I2.2,A2,I2.2,A1)')SIGNO,INT(ALH),'\\uo\\d ',INT(ALM),CHAR(39)//' ',INT(ALS),'"'
          CALL PLPTEXT(40.,YPGP,0.,0.,LINEA(1:18))
! epoca inicial
          WRITE(LINEA,'(A1,F6.1,A1)')'(',TII,')'
          CALL PLPTEXT(65.,YPGP,0.,0.,LINEA(1:8))
! ascension recta epoca final
          YPGP=YPGP-4.0
          CALL PLPTEXT(3.,YPGP,0.,0.,'\\ga:')
          CALL SEXAG(AR(I),ALH,ALM,ALS)
          WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),'m ',INT(ALS),'s'
          CALL PLPTEXT(8.,YPGP,0.,0.,LINEA(1:11))
! declinacion epoca final
          CALL PLPTEXT(35.,YPGP,0.,0.,'\\gd:')
          CALL SEXAG(DEC(I),ALH,ALM,ALS)
          SIGNO='+'
          IF(DEC(I).LT.0)THEN
            SIGNO='-'
            ALH=ABS(ALH)
            ALM=ABS(ALM)
            ALS=ABS(ALS)
          END IF
          WRITE(LINEA,'(A1,I2.2,A,I2.2,A2,I2.2,A1)')SIGNO,INT(ALH),'\\uo\\d ',INT(ALM),CHAR(39)//' ',INT(ALS),'"'
          CALL PLPTEXT(40.,YPGP,0.,0.,LINEA(1:18))
! epoca final
          WRITE(LINEA,'(A1,F6.1,A1)')'(',TFF,')'
          CALL PLPTEXT(65.,YPGP,0.,0.,LINEA(1:8))
! condiciones de la culminacion
          YPGP=YPGP-4.0
          CALL ORCASO(DEC(I),90+34./60.+ANGHOA)
          IF(ESTADO.EQ.'I')THEN
            CALL PLPTEXT(3.,YPGP,0.,0.,'Object is INVISIBLE')
          ELSE
            CALL PLPTEXT(3.,YPGP,0.,0.,'Culmination: ')
            WRITE(LINEA,'(F4.1,1X,A1)')ALTCUL,DONCUL
            CALL PLPTEXT(30.,YPGP,0.,0.,LINEA(1:6))
            CALL TIEMPOU(AR(I),0.,LONG,TS0,TUCULM)
            CALL SEXAG(TUCULM,ALH,ALM,ALS)
            WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),'m ',INT(ALS),'s'
          END IF
          CALL PLPTEXT(45.,YPGP,0.,0.,LINEA(1:11))
!***>
          IF((REAL(I)/NOBJEG.EQ.REAL(I/NOBJEG)).OR.(I.EQ.MT))THEN
          ELSE
            CALL PLSLS(3)
            CALL PLMOVE(1.,YPGP-2.)
            CALL PLDRAW(80.,YPGP-2.)
            CALL PLSLS(1)
          END IF
        END IF
        IF(OPCMEN.EQ.'2')THEN
!cc       WRITE(33,'(A8,I2,A17)')'Object #',NBODY,': '//NOM(I)
        END IF
        IF(OPCMEN.EQ.'3')THEN
          WRITE(LINEA,'(A,I2,A)')'#',NBODY,': '//NOM(I)
          YPGP=(41.-REAL(NBODY))*4.+14.
          CALL PLSCH(0.6)
          CALL PLPTEXT(225.,YPGP,0.,0.,LINEA(1:38))
        END IF
  550 CONTINUE
      CALL PLEND
      GOTO 300
  560 CONTINUE
!cc      CLOSE(UNIT=33)
!                                   ************************************
!                                   FORMATOS, ERRORES Y FIN DEL PROGRAMA
!                                   ************************************
  100 FORMAT(A,$)
  101 FORMAT(A)
  102 FORMAT(17X,'OBJETO',23X,'A.R.(',F6.1,')DEC.')
  103 FORMAT(17X,6('='),22X,4('='),8X,4('='))
  104 FORMAT(79('='))
  105 FORMAT(A,F8.4)
  110 FORMAT(A14,F6.1,A11,F6.1,A41)
  150 FORMAT(A30,',',F8.4,',',F8.4)
  160 FORMAT(6X,I2,4X,A20)
  170 FORMAT(A40,2(4X,F8.4))
  190 FORMAT(A,3(1X,I2.2,A1))
!
  900 CONTINUE
      STOP
  990 WRITE(*,101)'I/O ERROR with file: '//FILNAM
      STOP
      END
