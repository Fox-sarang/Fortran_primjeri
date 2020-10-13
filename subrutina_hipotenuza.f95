PROGRAM test_calc_hypotenuse
IMPLICIT NONE
! Data dictionary: declare variable types & definitions
REAL :: s1 ! Length of side 1
REAL :: s2 ! Length of side 2
REAL :: hypot ! Hypotenuse
! Get the lengths of the two sides.
WRITE (*,*) 'Program to test subroutine calc_hypotenuse: '
WRITE (*,*) 'Enter the length of side 1: '
READ (*,*) s1
WRITE (*,*) 'Enter the length of side 2: '
READ (*,*) s2
! Call calc_hypotenuse.
CALL calc_hypotenuse ( s1, s2, hypot )
! Write out hypotenuse.
WRITE (*,1000) hypot
1000 FORMAT ('The length of the hypotenuse is: ', F10.4 )
END PROGRAM test_calc_hypotenuse

SUBROUTINE calc_hypotenuse ( side_1, side_2, hypotenuse )
IMPLICIT NONE
REAL, INTENT(IN) :: side_1 ! Length of side 1
REAL, INTENT(IN) :: side_2 ! Length of side 2
REAL, INTENT(OUT) :: hypotenuse ! Length of hypotenuse
REAL :: temp ! Temporary variable
! Calculate hypotenuse
temp = side_1**2 + side_2**2
hypotenuse = SQRT ( temp )
END SUBROUTINE calc_hypotenuse
