!------------------------------------------------------------------------------
!                                                            File: endprogram.f
!
!------------------------------------------------------------------------------
!omment
!
! SUBROUTINE ENDPROGRAM(CADENA)
!
! Input: CADENA
!
! Stop the program
!
! CHARACTER*10 CADENA -> if CADENA='endprogram' STOP
!
!omment
!------------------------------------------------------------------------------
        SUBROUTINE ENDPROGRAM(CADENA)
        IMPLICIT NONE
        CHARACTER*10 CADENA
        CHARACTER*1 CSTOP
        IF(CADENA(1:10).EQ.'endprogram')THEN
          WRITE(*,100)'Do you want to STOP the program (y/n) [n] ?'
          READ(*,'(A)') CSTOP
          IF(CSTOP.EQ.'y')THEN
            CALL PLEND
            STOP
          END IF
        END IF
100     FORMAT(A,$)
        END
