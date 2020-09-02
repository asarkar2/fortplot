program main
use fortplot
implicit none

    real, dimension(:), allocatable :: x, y1, y2, y3, y4
    real :: pi, xmin, xmax, xh, ymin, ymax
    character(:), allocatable :: mplot_options
    integer :: i, n, ok

    ! Number of data points
    n = 200

    allocate(x(n), y1(n), y2(n), y3(n), y4(n), stat = ok)
    if (ok /= 0) stop "Couldn't allocate x, y1, y2, y3 or y4."

    pi = 4*atan(1.0)
    xmin = 0.0
    xmax = 2.5 * pi
    xh =  (xmax - xmin) / ( real(n) - 1 ) 

    x = [(xmin + (i-1)*xh, i=1,n)]    

    ymin = -1.5
    ymax = 1.5

    y1 = sin(x)
    y2 = sin(x**2)
    y3 = 1/(x+1)
    y4 = tan(x)

    call openplot()
    call figure(1)
    
    mplot_options = "layout 2,2 rowsfirst title 'Simple multiplot example'"
    call multiplot(.true., options = mplot_options) 

    ! While using multiplot use the 'set' commands after multiplot command.
    call xrange(xmin,xmax)
    call yrange(ymin,ymax)
    call gpcmd('set sample 300')
    call key("nobox noopaque font ',10'")
    call gpcmd("set tics font ',10'")

    call plot2d(x,y1, options="with lines", key = 'sin(x)')
    call plot2d(x,y2, options="with lines", key = 'sin(x^2)')
    call plot2d(x,y3, options="with lines", key = '1/(x+1)')
    call plot2d(x,y4, options="with lines", key = 'tan(x)')

    call multiplot(.false.)

    call saveas('multiplot_fig1.pdf')

    call closeplot()
!     call closeplot(delete = .false.)

end program main
