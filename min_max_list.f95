PROGRAM bad_format
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 10 ! Max velicina seta
INTEGER, DIMENSION(MAX_SIZE) :: input ! unos vrijednosti
INTEGER :: ilarge ! Pointer najvece vrijednosti
INTEGER :: ismall ! Pointer najmanje vrijednosti
INTEGER :: j ! DO loop index
INTEGER :: nvals ! Broj vrijednosti u setu
INTEGER :: temp ! Privremena varijabla
! Unos broja vrijednosti u setu
WRITE (*,*) 'Enter number of values in data set:'
READ (*,*) nvals
! Da li je vrojednost <= MAX_SIZE?
size: IF ( nvals <= MAX_SIZE ) THEN
 in: DO J = 1, nvals
 WRITE (*,100) 'Enter value ', j
 100 FORMAT (A,I3,': ')
 READ (*,*) input(j)
 END DO in
 ! Pronadi najvecu vrijednost
 temp = input(1)
 ilarge = 1
 large: DO j = 2, nvals
 IF ( input(j) > temp ) THEN
 temp = input(j)
 ilarge = j
 END IF
 END DO large
 ! Pronadi najmanju vrijednost
 temp = input(1)
 ismall = 1
 small: DO j = 2, nvals
 IF ( input(j) < temp ) THEN
 temp = input(j)
 ismall = j
 END IF
 END DO small
 ! Ispis vrijednosti
  WRITE (*,110)
 110 FORMAT ('The values are:')
 out: DO j = 1, nvals
 IF ( j == ilarge ) THEN
 WRITE (*,'(I6,2X,A)') input(j), 'LARGEST'
 ELSE IF ( J == ismall ) THEN
 WRITE (*,'(I6,2X,A)') input(j), 'SMALLEST'
 ELSE
 WRITE (*,'(I6)') input(j)
 END IF
 END DO out
 ELSE size
 ! nvals > max_size. Ispisi korisniku, fin
 WRITE (*,120) nvals, MAX_SIZE
 120 FORMAT (7('Too many input values: '), I6, ' > ', I6)
 END IF size
END PROGRAM
