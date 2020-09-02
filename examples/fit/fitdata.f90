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

    real, dimension(:), allocatable :: x, y
    integer :: i, n, ok
    real :: dx, a, b, c, xmin, xmax
    character(:), allocatable :: inparam, outparam, curve, nmlist
    
!     ! Default namelist is parameters
    namelist /parameters/ a, b, c
!     ! Change if required
    nmlist = 'parameters'
    
    n = 100
    allocate(x(n), y(n), stat = ok)
    if (ok /= 0) stop "Couldn't allocate x / y array."

    dx = 0.1 
    do i = 1,n
        x(i) = (i-1) * dx
        y(i) = sin(x(i)) + 0.1*rand(0)
    end do

    xmin = minval(x) ;      xmax = maxval(x)
!     print*, xmin, xmax

    ! Write the parameters to a file in the format given below.
    inparam = 'initial_parameters.txt'
    outparam = 'final_parameters.txt'

    ! Define the curve which is to be fitted.
    curve = 'a * cos( b + c * x )'
!     curve = 'a * sin( b + c * theta )'
    
    ! Default values of parameters
    a = 1.3 ; b = 0.2 ; c = 0.7 ;

    ! Write to input parameter file
    open(10, file = trim(inparam))
    write(10,*) "a = ", a ;     write(10,*) "b = ", b  ; 
    write(10,*) "c = ", c
    close(10)

    call openplot()
    call figure()

    ! Plot the data
    call gpcmd("set grid")
    call yrange(-1.5,1.5)
    call gpcmd("set key bottom right box")
    call xlabel("x")
    call ylabel("y")

    ! Fit the curve
    call fit(x,y,curve,infile = inparam,outfile = outparam)
    
    call hold(.true.)

    call plot2d(x,y,options = 'with points', key = 'Data')

    ! The outparam file from fit() becomes the input parameter file for cplot. 
    ! Plot the fitted curve
    call xrange(xmin, xmax)
    call cplot(curve, infile = outparam, key = 'Fitted function')

    call hold(.false.)

!     call gptofort(outparam,nmlist)
! 
!     open(20,file = outparam)
!     read(20,nml = parameters)
!     close(20)
! 
!     print*, "a = ", a
!     print*, "b = ", b
!     print*, "c = ", c

    call closeplot()

end program main
