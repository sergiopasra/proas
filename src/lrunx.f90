!------------------------------------------------------------------------------
!                                                                 File: lrunx.f
!
!------------------------------------------------------------------------------
!omment
!
! SUBROUTINE LRUNX(LRUN,LMANUAL)
!
! Output: LRUNX,LMANUAL
!
! Determine whether files .running_RUN and .running_MANUAL exist in current
! directory
!
! LOGICAL LRUN -> .TRUE. if file .running_RUN exist (.FALSE. otherwise)
! LOGICAL LMANUAL -> .FALSE. if file .running_MANUAL exist (.FALSE. otherwise)
!
!omment
!------------------------------------------------------------------------------
        SUBROUTINE LRUNX(LRUN,LMANUAL)
        IMPLICIT NONE
        LOGICAL LRUN,LMANUAL
        INQUIRE(FILE='.running_RUN',EXIST=LRUN)
        INQUIRE(FILE='.running_MANUAL',EXIST=LMANUAL)
        END
