module myfunctions
implicit none
contains

    function func3d(x,y) result(z)
        real, intent(in) :: x, y
        real :: z

        z = sin(x)*cos(y)

    end function func3d

end module myfunctions


program main
use myfunctions
use fortplot

implicit none

    real :: xmin, xmax, ymin, ymax

    call openplot()
    call terminal('wxt')
    call figure()

    call xlabel("x")
    call ylabel("y")
    call zlabel("z")

    xmin = -1.0 ;   xmax = 1.0
    ymin = -2.0 ;   ymax = 2.0
 
    call xrange(xmin, xmax)
    call yrange(ymin, ymax)

    call gpcmd("set hidden3d")
    call gpcmd("set ztics 0.3")

!     call gpcmd("set view 75, 50")

    call fplot3d(func3d,xmin,xmax,ymin,ymax,options = "with lines")

    call saveas('fplot3d_fig1.pdf')

!     call closeplot(delete = .false.)
    call closeplot()

end program main
