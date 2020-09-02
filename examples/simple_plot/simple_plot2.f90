! Plot Delta function on the fly
module my_functions
implicit none
contains 
    
    function func(x,g,a) result(y)
    real, dimension(:), allocatable, intent(in) :: x
    real, intent(in) :: g, a
    real, dimension(:), allocatable :: y
    real :: pi

        pi = 4.0*atan(1.0)
        y = sin(g*(x-a))/(pi*(x-a))

    end function func

end module my_functions


program main
    use my_functions
    use fortplot

    implicit none
    real, dimension(:), allocatable :: x, y
    real :: xmin, xmax, xh, a, g
    integer :: i, n, ok

    ! Gnuplot options
    character(:), allocatable :: label_text, ylabel_text

    xmin = 0
    xmax = 4.0
    n = 200
    xh =  (xmax - xmin) / ( real(n) - 1 ) 

    x = [(xmin + (i-1)*xh, i=1,n)]

    g = 20.0
    a = 2.0

    ! Get values of y array
    y = func(x,g,a)
!     print*, (x(i), y(i), new_line('c'), i = 1,n)

    ! Open gnuplot
    call openplot()

!     call terminal('wxt')
    call figure()

    ! For tex
    call xlabel("$x$")
    ylabel_text = "$\\sin(\\gamma(x-\\alpha))/(\\pi(x-a))$"
    call ylabel(ylabel_text)

    label_text = "$\\alpha$ = " // real2str(a,'f5.2') // ",\n" // &
!                 "g = " // num2str(r = g, w = 'f5.2')
                "$\\gamma$ = " // real2str(g,'f5.2')

    call label(1,label_text,0.15,0.8)

    call title('Plot of a graph')

    call xrange(xmin,xmax)

!     call plot2d(x,y)
!     call plot2d(x,y, options = "with lines")
    call plot2d(x,y, options = "with lines", key = 'f(x)')
!     call plot2d(x,y, options = "with lines linetype -1 linewidth 1.5", &
!                 key = 'f(x)')

    call saveas('simple_plot.tex', options = 'standalone')
!     call saveas('simple_plot.tex', options = 'pdf')
!     call saveas('simple_plot.tex', options = 'eps standalone')
!     call saveas('simple_plot.tex', driver = 'epslatex')
!     call saveas('simple_plot.tex', driver = 'epslatex', &
!                 options = 'standalone')

!     call slumber(0.2)

    ! Clean up
    call closeplot()

end program main

