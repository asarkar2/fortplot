program main
use fortplot
implicit none

    real :: xmin, xmax, ymin, ymax
    character(:), allocatable :: curve

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

    call gpcmd("set isosamples 40")
    call gpcmd("set hidden3d")
    call gpcmd("set ztics 0.3")

    call gpcmd("set view 75, 50")

    curve = 'sin(x)*cos(y)' 
    call cplot(curve,plot = 'splot')
!     curve = 'sin(p)*cos(q)' 
!     call cplot(curve,plot = 'splot',ivar = 'p,q')

    call closeplot()

end program main
