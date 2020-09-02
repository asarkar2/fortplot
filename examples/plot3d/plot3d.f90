! Simple 3D plot using plot3d() command
! Here z is 1d array
program main
use fortplot

    implicit none
    integer:: i, n, ok
    real, dimension(:), allocatable :: t, x, y, z
    real :: tmax, tmin, dt
    character(:), allocatable :: legend

    tmin = 0.0 ; tmax = 20.0 ; dt = 0.1
    n = floor( (tmax - tmin) / dt ) + 1
    t = [(tmin + (i-1)*dt, i = 1,n)]

    allocate(x(n), y(n), z(n), stat = ok)
    if (ok /= 0) stop "Couldn't allocate x, y or z arrays."
        
    x = t * cos(t)
    y = t * sin(t)
    z = -t**2

    call openplot()
!     
!     call terminal('x11')
    call terminal('wxt')
! !     call terminal('qt')
    call figure()
! 
!     ! Some special commands for splot
    call gpcmd('set ztics 0,100,400')
    call gpcmd('set view 60,45')

    call xlabel('x')
    call ylabel('y')
    call zlabel('z')

!     call gpcmd('set dgrid3d 30,30')
!     call gpcmd('set hidden3d')

    legend = 'Parametric plot'
    call plot3d(x,y,z,options = 'with lines', key = legend)

!     call slumber(5.0)

    call saveas('spiral.pdf')
    call closeplot()

end program main

