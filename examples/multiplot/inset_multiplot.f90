program main
use fortplot
    implicit none
    character(:), allocatable :: curve
    real :: pi

    pi = 4.0* atan(1.0)
    curve = 'exp(x)*sin(1/x)'

    call openplot()
    call figure()

    call multiplot(.true.)
    call xlabel("x")
    call ylabel("y")

    call grid(.true.)
    call gpcmd("set samples 1000")
    call gpcmd("set xtics 0.4")
    call gpcmd("set ytics 10")
    call xrange(0.0,2*pi)
    call gpcmd("set arrow from 0.1, 2.1 to screen 0.22, 0.4 front linetype -1")
    call key("box spacing 1.2")
    call cplot(curve,key = curve)

    ! Inset graph begins
    call gpcmd("set origin 0.2, 0.4")
    call gpcmd("set size 0.25, 0.25")
    call gpcmd("clear")
    call gpcmd("unset xlabel")
    call gpcmd("unset ylabel")
    call key("off")
    call gpcmd("unset grid")
    call gpcmd("unset object")
    call gpcmd("unset arrow")
    call gpcmd("set xtics 0.1")
    call gpcmd("set ytics 0.6")
    call gpcmd("set margin 3, 1, 1, 1")
    call xrange(0.0,0.2)

    call gpcmd("replot")
!     call cplot(curve)
    ! Inset graph ends

    call multiplot(.false.)

    call saveas('inset_multiplot_fig1.pdf')

!     call closeplot(delete = .false.)
    call closeplot()

end program main
