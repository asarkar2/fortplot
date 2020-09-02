module my_subroutines
implicit none
contains 
    
    ! Simple sine function
    function func1(x,ai) result(y)
    real, dimension(:) :: x
    real, dimension(:), allocatable :: y
    real, intent(in) :: ai

        y = sin(x-ai)

    end function func1


    ! Second function
    function func2(x,ai) result(y)
    real, dimension(:) :: x
    real, dimension(:), allocatable :: y
    real, intent(in) :: ai

        y = sin(x+ai)

    end function func2

end module my_subroutines

program main
use fortplot
use my_subroutines

    implicit none

    integer:: nmax, an, i, j, ok
    real:: xmax, xmin, xh
    real, dimension(:), allocatable :: x, y1, y2, a
    real :: amin, amax, ah
    character(100) :: label_text, to_terminal

    nmax = 150
    an = 10

    allocate(x(nmax), y1(nmax), y2(nmax), a(an), stat = ok)
    if (ok /= 0) stop "Couldn't allocate x, y1, y2, or a"

    xmax = 7.0
    xmin = -7.0
    xh   = ( xmax - xmin ) / (real(nmax) - 1) 
    x    = [(xmin + (i-1)*xh, i=1,nmax)]

    amin = 0
    amax = 4
    ah = ( amax - amin ) / (real(an) - 1) 
    a = [(amin + (j-1)*ah, j=1, an)]

    ! Start the plot
    call openplot()
    call figure()

    ! Any gnuplot commands can be passed using the gpcmd command
    call gpcmd("set zeroaxis")
    call gpcmd("set autoscale fix")
    call key("box opaque bottom left")

        ! Note that the limits are real. So pass 0.0 instead of 0
!     call xrange(0.,2.)
!     call yrange(0.,1.)
    
    do j = 1, an

!       Data generation
        y1 = func1(x,a(j))
        y2 = func2(x,a(j))

        write(*,*) "j = ", j, ", a = ", a(j)

        call gpcmd('set style textbox opaque')
        write(label_text,'(a,i3,a,f4.2,$)') "j = ",j,"\na = ",a(j)
        call label(1,label_text,0.85,0.82,options = 'boxed')

        ! Special subroutines have been provided to set the labels & title.
        call xlabel('x')
        call ylabel('sin(x-a), sin(x+a)')
        call title('My hold on graph plots')
    
        ! To plot multiple plots on the same graph
        call hold(.true.)

        call plot2d(x,y1, options = "with lines", key = 'sin(x-a)')

        call plot2d(x,y2, options = "with lines dashtype 2", key = 'sin(x+a)')
!         call plot2d(x,y1, datafile = 'data1.txt', & 
!             options = "with lines title 'sin(x-a)'")
!         call plot2d(x,y2, datafile = 'data2.txt', &
!             options = "with lines title 'sin(x+a)'")
!         call gpcmd('# Before hold false')
    
        ! To switch off plotting multiple plots on the same graph
        call hold(.false.)

        ! Slow the program, so that the plot window can be updated.
        call slumber(0.2)

    end do

    call saveas("hold2.gif", options = 'animate delay 0.2 loop 0')

    ! Clean up
    call closeplot()
    ! Gnuplot commands end

end program main

