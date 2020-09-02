! Create multipage pdf output
module my_subroutines
implicit none
contains 
    
    ! Simple sine function
    subroutine func(x,y,ai)
    real, dimension(:), intent(in) :: x
    real, dimension(:), intent(out) :: y
    real, intent(in) :: ai

        y = sin(x-ai)

    end subroutine func


end module my_subroutines

program main
use fortplot
use my_subroutines

    implicit none
    integer, parameter:: nmax = 150, an = 10
    integer:: i
    real:: xmax, xmin, xh
    real, dimension(nmax) :: x
    real, dimension(nmax) :: y
    real :: amin, amax, ah
    real, dimension(an) :: a
    real :: ymin, ymax
    character(100) :: label_text, to_terminal

    xmax = 7.0
    xmin = -7.0
    xh   = ( xmax - xmin ) / (real(nmax) - 1) 
    x    = [(xmin + (i-1)*xh,i=1,nmax)]

    ymin = -1.5
    ymax = 1.5

    amin = 0
    amax = 4
    ah = ( amax - amin ) / (real(an) - 1) 
    a = [(amin + (i-1)*ah,i=1,an)]

    ! Start the plot 
    call openplot()
    call figure()
    call gpcmd('set terminal pdfcairo enhanced color notransparent')
    call gpcmd("set output 'multipage_pdf_output.pdf'")
    
    ! Any gnuplot commands can be passed using the gpcmd() command
    call gpcmd("set zeroaxis")
    call gpcmd("set autoscale fix")
!     call gpcmd("set key nobox noopaque")
    call gpcmd("set key box bottom left opaque")

        ! Note that the limits are real. So pass 0.0 instead of 0
!     call xrange(0.,2.)
!     call yrange(0.,1.)
    
    do i = 1, an

!       Data generation
        call func(x,y,a(i))

        write(*,'(a,i3,a,f6.3)') "i = ", i, ", a = ",a(i) 
!         write(to_terminal,'(a,i3,a,f6.3)') "i = ", i, ", a = ",a(i) 
!         call gpcmd(to_terminal)

        write(label_text,'(a,i3,a,f4.2,$)') "i = ",i,"\na = ",a(i)
        call gpcmd('set style textbox opaque')
        call label(1,label_text,0.85,0.85,options = 'boxed')

        ! Special subroutines have been provided to set the labels & title.
        call xlabel('x')
        call ylabel('sin(x-a)')
    
        call plot2d(x,y, options = "with lines", key = "sin(x-a)")
!         call plot2d(x,y, datafile = 'data1.txt', & 
!             options = "with lines title 'sin(x-a)'")

!         print*,
        ! Gnuplot commands end

        ! Slow the program, so that the plot window can be updated.
        call slumber(0.1)
!         call cleanup()

    end do

    ! To flush the output
    call gpcmd('unset output')
    call gpcmd('unset terminal')

    ! Clean up
!     call slumber(1.0)
!     call cleanup()
    call closeplot()

end program main

