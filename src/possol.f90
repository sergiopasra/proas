! **********************************************************************
!                                                      SUBROUTINE POSSOL
!                                                      *****************
!
      SUBROUTINE POSSOL(IAP)
!
! ---------------------------------------------------------------
!
!
!          Posicion del Sol, J. Meeus
!          Astronomical Formulae for Calculators
!          Cap. 18, pag. 79 y siguientes
!
!          Subrutinas escritas por M. Cornide
! ---------------------------------------------------------------
! IAP=1 ---> coordenadas aparentes
! IAP=0 ---> coordenadas no aparentes
!
      IMPLICIT NONE
!---> argumentos ficticios
      INTEGER IAP
!---> variables globales: ENTRADA
      REAL*8 FJ
!---> variables globales: SALIDA
      REAL ARSUN,DECSUN
!---> variables locales
      REAL*8 PI,RAG
      REAL*8 ALAMBDA,PAR
      REAL*8 AR,DEC
!---> common blocks
      COMMON/BLKFJ2/FJ
      COMMON/BPOSOL/ARSUN,DECSUN
!
      PI=3.141592654D0
      RAG=180.0D0/PI
      CALL PSOL(FJ,IAP,ALAMBDA,PAR)
      CALL ECUAT(FJ,ALAMBDA,0.D0,AR,DEC)
      ARSUN=AR*RAG/15.D0
      DECSUN=DEC*RAG
      END
! ********************************************************************
!
!
      SUBROUTINE PSOL(FJ,IAP,ALON,PAR)
!
!             AL : Long. media
!             AM : Anomalia media
!             EXC: Excentricidad
!             EC : Ecuacion del centro
!             V  : Anomalia verdadera
!             R  : Radiovector
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PI=3.141592654D0
      RAG=180.0D0/PI
      DPI=2.0D0*PI
      T=(FJ-2415020.D0)/36525.D0
!
         AL=279.69668D0+36000.76892D0*T+.0003025D0*T*T
         AM=358.47583D0+35999.04975D0*T-0.000150D0*T*T&
          &+.0000033D0*T*T*T
         AL=DMOD(AL,360.D0)/RAG
         AM=DMOD(AM,360.D0)/RAG
         EXC=.016751014D0-.0000418D0*T-.000000126D0*T*T
         C1=1.919460D0-.004789D0*T-.000014D0*T*T
         C2=.020094D0-.000100D0*T
         EC=(C1*DSIN(AM)+C2*DSIN(2.*AM)+.000293D0*DSIN(3.*AM))/RAG
!        !
         ALON=AL+EC
         V=AM+EC
         R=1.0000002D0*(1.D0-EXC*EXC)/(1.D0+EXC*DCOS(V))
!        !
!        !    Perturbaciones
!        !
         A=(153.23D0+22518.7541D0*T)/RAG
         B=(216.57D0+45037.5082D0*T)/RAG
         C=(312.69D0+32964.3577D0*T)/RAG
         D=(350.74D0+445267.1142D0*T-.00144D0*T*T)/RAG
         E=(231.19D0+20.20D0*T)/RAG
         H=(353.40D0+65928.7155D0*T)/RAG
!        !
         A=DMOD(A,DPI)
         B=DMOD(B,DPI)
         C=DMOD(C,DPI)
         D=DMOD(D,DPI)
         H=DMOD(H,DPI)
         E=DMOD(E,DPI)
!        !
         ALON=ALON+(.00134D0*DCOS(A)+.00154D0*DCOS(B)&
                 &+.00200D0*DCOS(C)+.00179D0*DSIN(D)&
                 &+.00178D0*DSIN(E))/RAG
         R=R+.00000543D0*DSIN(A)+.00001575D0*DSIN(B)&
           &+.00001627D0*DSIN(C)+.00003076D0*DCOS(D)&
           &+.00000927D0*DSIN(H)
!
         IF(IAP .EQ. 1) THEN
            OM=(259.18D0-1934.142D0*T)/RAG
            OM=DMOD(OM,DPI)
            IF(OM .LT. 0.) OM=OM+DPI
            ALON=ALON-(0.00569D0+0.00479D0*DSIN(OM))/RAG
         ENDIF
         RR=R*1.49597870D11/6.37814D6
         PAR=ASIN(1.D0/RR)*RAG
!
      RETURN
      END
!
!
!
!
      SUBROUTINE EPSILON(FJ,EPSI)
!
!
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!
!
!  ----- Calculo del parametro U:
!
      U=(FJ-2451545.)/3652500.
!
!  ----- Calculo de la oblicuidad de la ecliptica
!
      A1=2.18D0-3375.70D0*U+0.36D0*U**2
      A2=3.51D0+125666.39D0*U+0.10D0*U**2
      EPSI=0.4090928D0-0.0226938D0*U-75.D-7*U**2+96926.D-7*U**3&
      &-2491.D-7*U**4-12104.D-7*U**5+(446*DCOS(A1)+28*DCOS(A2))*1.D-7
      RETURN
      END
!
!
!
!
      SUBROUTINE ECUAT(FJ,AL,B,A,D)
!
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      EXTERNAL EPSILON
!
!           Calcula  Ascension Recta y Declinacion
!
      DPI=6.283185307D0
      CALL EPSILON(FJ,EPS)
!
      X0=DCOS(B)*DCOS(AL)
      Y0=DCOS(B)*DSIN(AL)
      Z0=DSIN(B)
!
      X=X0
      Y=Y0*DCOS(EPS)-Z0*DSIN(EPS)
      Z=Y0*DSIN(EPS)+Z0*DCOS(EPS)
!
      A=ATAN2(Y,X)
      IF(A .LT. 0.) A=A+DPI
      D=ASIN(Z)
!
      RETURN
      END
