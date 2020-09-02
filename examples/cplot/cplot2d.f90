program main
use fortplot
implicit none

    character(:), allocatable :: curve, inparam
    character(300) :: label_str
    real :: xmin, xmax, a, b, c

    a = 0.9     
    b = 0.0019   
    c = 1.0039

    curve = 'a*exp(- (x-b)**2 / (2*c**2))' 

    ! Write to input parameter file
    inparam = 'parameters.txt'
    open(10, file = trim(inparam))
    write(10,*) "a = ", a ;     write(10,*) "b = ", b  ; 
    write(10,*) "c = ", c
    close(10)    

    xmin = -5.0 ;       xmax = 5.0
    
    call openplot()
    call figure()

    call xlabel("x")
    call ylabel("y")

    call xrange(xmin,xmax)

    write(label_str,'(a,f6.4,a,f6.4,a,f6.4)') "a = ", a , &
            "\nb = ", b, "\nc = ", c
    call label(1,trim(label_str),0.15,0.90)
    
    call grid(.true.)

    call hold(.true.)
    call cplot(curve,infile = inparam)
    call cplot('sin(x)')
    call hold(.false.)

    call closeplot()

end program main
