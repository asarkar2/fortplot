module myfunc
implicit none
contains

    elemental function func(xi,yj) result(zij)
        real, intent(in) :: xi, yj
        real :: zij

        zij = sin(sqrt(xi**2 + yj**2)) / sqrt(xi**2+yj**2)
!         zij = (xi**2 - yj**2)

    end function func

end module myfunc

program main
use myfunc
use fortplot

    implicit none
    real, dimension(:), allocatable :: x, y
    real, dimension(:,:), allocatable :: z
    integer :: i, j, n, m, ok
    real :: xmin, xmax, dx, ymin, ymax, dy

    xmin = -4 ; xmax = 4 ; dx = 0.1 
    ymin = -3 ; ymax = 3 ; dy = 0.15 

    n = floor((xmax - xmin)/dx) + 1
    m = floor((ymax - ymin)/dy) + 1

    allocate(x(n), y(m), z(n,m), stat = ok)
    if (ok /= 0) stop "Couldn't allocate x or y"

    x = [(xmin + (i-1)*dx, i = 1, n)]
    y = [(ymin + (j-1)*dy, j = 1, m)]

    do i = 1, n
        do j = 1, m
            z(i,j) = func(x(i), y(j))
        end do
    end do

!     call openplot(debug = .true.)
    call openplot()
    
    call figure()
    call gpcmd("set hidden3d")
    call plot3dg(x,y,z,options = 'with lines')
    call saveas('plot3dg_fig1.pdf')

    call figure()
    call gpcmd("set cntrparam levels incremental -0.2, 0.2, 1")
    call gpcmd("set hidden3d")
    call contour(x,y,z, filled = .true., key = 'Contour plot')
    call saveas('countour_fig1.pdf')

    call closeplot()
!     call closeplot(delete = .false.)

end program main

