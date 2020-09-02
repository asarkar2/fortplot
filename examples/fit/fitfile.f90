! TODO:
! *) [D] Separate out fitting and plotting
! *) [D] Make namelist optional, with default value
! *) [D] Make output file optional
! *) [D] Data options
! *) [D] Fitted curve options
! *) [D] Change independent variable
! *) [D] Pass x and y, instead of datafile
! *) [D] Data key
! *) [D] Fitcurve key
 
program main
use fortplot
implicit none

    character(:), allocatable :: datafile, curve, inparam, outparam
    real :: a, b, c, xmin, xmax

!     ! Datafile and parameters
!     datafile = 'hist_table_count.txt'
!     a = 1500 ; b = 0.5 ; c = 0.7 ;  ! For count
    
    ! Datafile and parameters
    datafile = 'hist_table_pdf.txt'
    a = 1.0 ; b = 0.5 ; c = 0.7 ;  ! For pdf

    ! Define the curve which is to be fitted.
    ! Gaussian function
    ! https://en.wikipedia.org/wiki/Gaussian_function
    curve = 'a * exp(- (x-b)**2 / (2*c**2))' 

    ! Write the parameters to a file in the format given below.
    inparam = 'hist_parameters.txt'
    outparam = 'output_parameters.txt'

    ! Write to input parameter file
    open(10, file = trim(inparam))
    write(10,*) "a = ", a ;     write(10,*) "b = ", b  ; 
    write(10,*) "c = ", c
    close(10)

    call openplot()
    call figure()

    ! Plot the data
    call grid(.true.)
    call xlabel("x")
    call ylabel("y")

!     ! Fit the curve
!     call fitfile(datafile,curve,inparam)
    call fitfile(datafile,curve,inparam,outfile = outparam)

    call hold(.true.)
    call plotfile(datafile, options = 'using 1:2 with points', &
                        key = 'Data')
! 
    xmin = -5.0 ; xmax = 5.0
    call xrange(xmin,xmax)

    ! Plot the fitted curve
    call cplot(curve,infile = outparam)

    call hold(.false.)

    call saveas('histogram.pdf')

    call gpcmd("fwhm = 2*sqrt(2*log(2)) * c")
    call gpcmd("print 'FWHM = ', fwhm")

    call closeplot()

end program main
