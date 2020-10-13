PROGRAM stats_1
IMPLICIT NONE

INTEGER, PARAMETER :: MAX_SIZE = 100

REAL, DIMENSION(MAX_SIZE) :: a
LOGICAL :: exceed = .FALSE.

CHARACTER(len=20) :: filename
INTEGER :: i
INTEGER :: iptr
INTEGER :: j
REAL :: median
CHARACTER(len=80) :: msg
INTEGER :: nvals = 0
INTEGER :: status
REAL :: std_dev
REAL :: sum_x = 0.
REAL :: sum_x2 = 0.
REAL :: temp
REAL :: x_bar

WRITE (*,1000)
1000 FORMAT ('Enter the file name with the data to be processed: ')
READ (*,'(A20)') filename

OPEN ( UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
 IOSTAT=status, IOMSG=msg )

fileopen: IF ( status == 0 ) THEN
 DO
 READ (9, *, IOSTAT=status) temp
 IF ( status /= 0 ) EXIT
 nvals = nvals + 1
 size: IF ( nvals <= MAX_SIZE ) THEN
 a(nvals) = temp
 ELSE
 exceed = .TRUE.
 END IF size
 END DO

 toobig: IF ( exceed ) THEN
 WRITE (*,1010) nvals, MAX_SIZE
 1010 FORMAT ('Maximum array size exceeded: ', I0, ' > ', I0 )
 ELSE
  outer: DO i = 1, nvals-1
  iptr = i
 inner: DO j = i+1, nvals
 minval: IF ( a(j) < a(iptr) ) THEN
 iptr = j
 END IF minval
 END DO inner

 swap: IF ( i /= iptr ) THEN
 temp = a(i)
 a(i) = a(iptr)
 a(iptr) = temp
 END IF swap
 END DO outer

 sums: DO i = 1, nvals
 sum_x = sum_x + a(i)
 sum_x2 = sum_x2 + a(i)**2
 END DO sums

 enough: IF ( nvals < 2 ) THEN

 WRITE (*,*) ' At least 2 values must be entered.'
 ELSE

 x_bar = sum_x / real(nvals)
 std_dev = sqrt( (real(nvals) * sum_x2 - sum_x**2) &
 / (real(nvals) * real(nvals-1)) )
 even: IF ( mod(nvals,2) == 0 ) THEN
 median = ( a(nvals/2) + a(nvals/2+1) ) / 2.
 ELSE
 median = a(nvals/2+1)
 END IF even

 WRITE (*,*) 'The mean of this data set is: ', x_bar
 WRITE (*,*) 'The median of this data set is:', median
 WRITE (*,*) 'The standard deviation is: ', std_dev
 WRITE (*,*) 'The number of data points is: ', nvals
 END IF enough
 END IF toobig
ELSE fileopen

 WRITE (*,1050) TRIM(msg)
 1050 FORMAT ('File open failed--error = ', A)
END IF fileopen
END PROGRAM stats_1
