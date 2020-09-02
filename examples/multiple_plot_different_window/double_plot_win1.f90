module my_subroutines
implicit none
contains 
    
    ! Complex subroutine
    subroutine cfunc(x,y,ai)
    real, dimension(:), intent(in) :: x
    complex, dimension(:), intent(out) :: y
    real, intent(in) :: ai

    y = exp(ai*cmplx(0,x)) + exp(ai*cmplx(0,2*x))/2.0
    end subroutine cfunc

end module my_subroutines

program main
use fortplot
use my_subroutines

    implicit none
    integer, parameter:: nmax = 150
    integer:: i
    real:: xmax, xmin, xh
    real, dimension(nmax) :: x
    complex, dimension(nmax) :: fx
    real :: a
    character (30):: key1, key2

    xmax = 7.0
    xmin = -7.0
    xh = ( xmax - xmin ) / (real(nmax) - 1) 
    x = [(xmin + (i-1)*xh,i=1,nmax)]
    
    a = 1.5

    key1 = "Re(f(x))"
    key2 = "Im(f(x))"

!   Data generation
    call cfunc(x,fx,a)

    call openplot()

!     call terminal('x11')

    ! 1st plot
    call figure(1)
    call gpcmd("set autoscale fix")
    call gpcmd("set key box opaque")
    call xlabel('x')
    call ylabel('Real f(x)')
    call plot2d(x,real(fx), options = "with lines linestyle 1", &
                key = 'Re(f(x))')

    call saveas('realfx.pdf')

    ! 2nd plot
    call figure(2)
    call gpcmd("set autoscale fix")
    call gpcmd("set key box opaque")
    call xlabel('x')
    call ylabel('Imag f(x)')
    call plot2d(x,aimag(fx), options = "with lines linestyle 2", & 
                key = 'Im(f(x))')

    call saveas('imagfx.pdf')

    call closeplot()
!     call closeplot(delete = .false.)

end program main

