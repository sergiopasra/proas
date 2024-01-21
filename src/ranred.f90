!------------------------------------------------------------------------------
!omment
!
! REAL FUNCTION RANRED(NSEED)
!
! Input: NSEED
! Output: RANRED (function)
!
! Return a random number in the range [0,1) using the intrinsic fortran
! function RAND(). If NSEED<0 a previous call to SRAND(TIME()) is also 
! performed.
!
! INTEGER NSEED -> NSEED=0: RANRED returns the next random number in the
!                           sequence.
!                  NSEED<0: RANRED performs a previous call to the
!                           intrinsic fortran function SRAND(TIME()), and
!                           generates a random number in the new sequence.
!                           In this case NSEED returns 0.
!                  NSEED>0: RANRED performs a previous call to the
!                           intrinsic fortran function SRAND(NSEED), and
!                           generates a random number in the new sequence.
!                           In this case NSEED returns 0.
!
!omment
!------------------------------------------------------------------------------
        REAL FUNCTION RANRED(NSEED)
        IMPLICIT NONE
        INTEGER NSEED
!------------------------------------------------------------------------------
        IF(NSEED.EQ.0)THEN
          RANRED=RAND()
          RETURN
        ELSEIF(NSEED.LT.0)THEN
          CALL SRAND(TIME())
        ELSEIF(NSEED.GT.0)THEN
          CALL SRAND(NSEED)
        END IF
        NSEED=0
        RANRED=RAND()
!
        END
