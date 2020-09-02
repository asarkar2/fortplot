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
    integer, parameter:: nmax = 150, an = 10
    integer:: i
    real:: xmax, xmin, xh
    real, dimension(nmax) :: x
    real, dimension(nmax) :: y1, y2
    real :: a
    character(100) :: label_text, to_terminal

    xmax = 7.0
    xmin = -7.0
    xh   = ( xmax - xmin ) / (real(nmax) - 1) 
    x    = [(xmin + (i-1)*xh,i=1,nmax)]

    a = 2.5
    
    !   Data generation
    y1 = func1(x,a)
    y2 = func2(x,a)

    ! Start the plot
    call openplot()
!     call openplot(debug = .true.)
    call figure()

    ! Any gnuplot commands can be passed using the gpcmd() command
    call gpcmd("set zeroaxis")
    call gpcmd("set autoscale fix")
    call key("box opaque bottom left")

    ! Note that the limits are real. So pass 0.0 instead of 0
!     call xrange(0.,2.)
!     call yrange(0.,1.)

    write(*,*) "a = " , a 

!     call gpcmd('# After hold on')
    call gpcmd('set style textbox opaque')
    write(label_text,'(a,f4.2,$)') "a = ",a
!     write(label_text,*) "a = ",real2str(a)
    call label(1,label_text,0.85,0.82, options = 'boxed')

    ! Special subroutines have been provided to set the labels & title.
    call xlabel('x')
    call ylabel('sin(x-a), sin(x+a)')
    call title('Graph plots')
    
    ! To plot multiple plots on the same graph
    call hold(.true.)
    
    call plot2d(x,y1, options = "with lines", key = 'sin(x-a)')
    
    call plot2d(x,y2, options = "with lines dashtype 2", key = 'sin(x+a)')
    
    ! To switch off plotting multiple plots on the same graph
    call hold(.false.)

    call saveas('hold1.pdf')

    ! Slow the program, so that the plot window can be updated.
    call closeplot()
    ! Gnuplot commands end

end program main

