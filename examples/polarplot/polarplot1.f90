program main
use fortplot

    implicit none
    real :: pi, dtheta
    real, dimension(:), allocatable :: rho, theta
    integer :: i, n, ok

    pi = 4.0 * atan(1.0)
    dtheta = 0.01 
    n = floor((2*pi - 0 ) / dtheta) + 1
!     print*, "n = ",n

    allocate(rho(n), theta(n), stat = ok)
    if (ok /= 0) stop "Couldn't allocate rho, theta"

    ! theta is in radians
    theta = [(0+(i-1)*dtheta, i = 1,n)]
    rho = sin(2*theta)*cos(2*theta)

    call openplot()
!     call terminal('wxt')
    call figure()

    call xrange(-0.5,0.5)
    call yrange(-0.5,0.5)

    call polarplot(theta,rho, options = "with lines", key = "Polar plot" )

    call saveas("polarplot1.eps")
!     call saveas("polarplot1.pdf")

    call closeplot()
!     call closeplot(delete = .false.)

end program main

