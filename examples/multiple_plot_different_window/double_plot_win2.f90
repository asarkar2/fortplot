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
    integer, parameter:: nmax = 150, an = 10
    integer:: i
    real:: xmax, xmin, xh
    real, dimension(nmax) :: x
    complex, dimension(nmax) :: fx
    real :: amin, amax, ah
    real, dimension(an) :: a
    real :: ymin, ymax
    character (30):: key1, key2

    xmax = 7.0
    xmin = -7.0
    xh = ( xmax - xmin ) / (real(nmax) - 1) 
    x = [(xmin + (i-1)*xh,i=1,nmax)]

    ymin = -1.5
    ymax = 1.5

    amin = 1
    amax = 2
    ah = ( amax - amin ) / (real(an) - 1) 
    a = [(amin + (i-1)*ah,i=1,an)]

    key1 = "Re(f(x))"
    key2 = "Im(f(x))"

    call openplot()

    do i = 1, an

!       Data generation
        call cfunc(x,fx,a(i))
        
        ! Plots
        call figure(1)
        call gpcmd("set autoscale fix")
        call xlabel('x')
        call ylabel('Re(f(x))')
        call plot2d(x,real(fx),  options = "with lines linestyle 1", &
                    key = 'Re(f(x))')

        call figure(2)
        call gpcmd("set autoscale fix")
        call xlabel('x')
        call ylabel('Im(f(x))')
        call plot2d(x,aimag(fx), options = "with lines linestyle 2", &
                    key = 'Im(f(x))')

        call slumber(0.1)
    end do

    call saveas('realfx.gif', savefig = 1, options = "animate delay 0.2 loop 0")
    call saveas('imagfx.gif', savefig = 2, options = "animate delay 0.2 loop 0")

    ! Clean up auto-generated data files
    call closeplot()
!     call closeplot(delete = .false.)

end program main

