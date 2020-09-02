! Plot Delta function on the fly
module my_functions
implicit none
contains 
    
    function func(x,g,a) result(y)
    real, dimension(:), intent(in) :: x
    real, intent(in) :: g, a
    real, dimension(:), allocatable :: y
    real :: pi

        pi = 4.0*atan(1.0)
        y = sin(g*(x-a))/(pi*(x-a))

    end function func

end module my_functions


program main
    use fortplot
    use my_functions

    implicit none
    real, dimension(:), allocatable :: x, y
    real, dimension(:), allocatable :: g
    real :: xmin, xmax, xh, a, gmin, gmax, gh 
    integer :: i, n, m

    ! Gnuplot options
    character(:), allocatable :: label_text
    character(:), allocatable :: ylabel_str

    n = 200
    m = 16
    xmin = 0
    xmax = 4.0
    xh =  (xmax - xmin) / ( real(n) - 1 ) 
    x = [(xmin + (i-1)*xh,i=1,n)]

    gmin = 5.0
    gmax = 20.0
    gh = (gmax - gmin) / ( real(m) - 1 )
    g = [(gmin + (i-1)*gh,i=1,m)]

    a = 2.0

    call openplot()
    call figure()
    call xrange(xmin,xmax)
    call yrange(-2.0,7.0)
    
    call xlabel("x")
    ylabel_str = "sin(g(x-a))/({/Symbol p}(x-a))"
    call ylabel(ylabel_str)

    call title('Plot of a graph')
    
    do i = 1, m

        ! Get values of y array
        y = func(x,g(i),a)

        label_text = "a = " // real2str(a,'f5.2') // "\n" &
                    // "g = " // real2str(g(i),'f5.2')
        call label(1,label_text,0.15,0.8)

        call plot2d(x,y, options = "with lines", key = 'f(x)')

        ! Slow down so that gnuplot can plot
        call slumber(0.2)
    end do

    call saveas('simple_plot3.gif', options = "animate delay 0.2 loop 0")

    ! Clean up
    call closeplot()

end program main

