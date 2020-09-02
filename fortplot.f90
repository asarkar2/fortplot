! Module to plot via gnuplot from fortran on the fly

! To compile
! gcc -c cprint.c               # Creates cprint.o
! gfortran -c fortplot.f90      # Creates fortplot.o
! gfortran -c your_program.f90  # Creates your_program.o
! gfortran cprint.o fortplot.o your_program.o
!       # Creates a.out (linux) 
!       # Creates a.exe (windows) 
!
! To run
! ./a.out # (linux)
! a.exe   # (windows)

! Author: Anjishnu Sarkar
! Version: 9.42

module fortplot
use iso_c_binding, only: C_CHAR, C_NULL_CHAR
implicit none

character(10), save :: TERM_FINAL   = 'qt'

character(len=*), parameter :: GPDIR = 'gnuplot_files'
character(len=*), parameter :: SEP = '/'    ! Path separator

! All temporary files are saved in GPDIR.
character(len=*), parameter :: DEXT = 'dat' ! Temporary array data file
character(len=*), parameter :: GEXT = 'plt' ! Temporary gnuplot script file
character(len=*), parameter :: FEXT = 'fmp' ! Temporary fortran readable file

character(50), save :: TERMINAL_OPTIONS = 'noraise enhanced'
logical, save :: HOLD_ON = .false.
logical, save :: DEBUG_ON = .false.

integer, parameter :: FIGID_INITIAL = 0         ! Figure window id
integer, save :: FIGID_FINAL = FIGID_INITIAL

integer, parameter :: DATAFID_INITIAL = 400     ! Data file id
integer, save :: DATAFID = DATAFID_INITIAL 

integer, parameter :: HFID_INITIAL = 800        ! Gnuplot hold file id
integer, save :: HFID = HFID_INITIAL

integer, parameter :: FORTFID_INITIAL = 1000    ! Fortran file id
integer, save :: FORTFID = FORTFID_INITIAL

integer, save :: HOLD_COUNT = 0

private :: print2file, get_integer, replace_text, rindex, reverse_string, &
            set_parameter, set_parameter_text, xyzrange, get_plot_cmd, &
            hist_common1, hist_common2, no_driver_support, datafile_xyz_grid

contains 

    subroutine terminal(term)
    ! Set the terminal of your choice
    ! Input : 
    !   term: Mandatory, character. Set the terminal name. 
    !         Example: 'wxt', 'qt'
    ! Output: None

        character(len=*), intent(in) :: term
        TERM_FINAL = trim(term)
    end subroutine terminal


    subroutine figure(figid)
    ! Start a gnuplot figure

        integer, intent(in), optional :: figid
        character(:), allocatable :: set_term, errmsg, gpfile
        logical :: fileopen
        integer :: stat        
        
        FIGID_FINAL = get_integer(FIGID_FINAL+1, figid)

        gpfile = GPDIR // SEP // "fig_" // int2str(FIGID_FINAL,'i0.6') &
                    // '.' // GEXT 
!         print*, gpfile
        inquire(file = gpfile, opened = fileopen)
        if (.not. fileopen) then
            open(FIGID_FINAL, file = gpfile, iostat = stat)
            if (stat /= 0) stop "Couldn't open the gnuplot file for writing."
        end if

        call gpcmd('reset', tofile = .false.)
        set_term = "set terminal " // trim(TERM_FINAL) // " " // &
                    int2str(FIGID_FINAL) // " " // trim(TERMINAL_OPTIONS)
        call gpcmd(set_term, tofile = .false.)

    end subroutine figure


    subroutine plot2d(x, y, options, key)
    ! Plot a 2D figure from x and y arrays

        real, dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: options, key

        integer :: xn, yn, j
        character(6)   :: plot_cmd
        character(:), allocatable :: datafilepath

        ! Initialize values
        plot_cmd = 'plot'

        ! Find the array sizes of the two arrays
        xn = size(x)
        yn = size(y)

        ! Check whether arrays match in size or not
        if (xn /= yn) then
            stop "Supplied arrays do not match in size"
        end if

        DATAFID = DATAFID + 1
        
        ! Create the data filename path
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT

        open(DATAFID, file=trim(datafilepath))
        write(DATAFID,*) "# x   y"
        write(DATAFID,*) (x(j),y(j),NEW_LINE('C'),j=1,xn)
        close(DATAFID)

        call plotfile(plot = plot_cmd, datafile = trim(datafilepath), &
                      options = trim(options), key = trim(key))

    end subroutine plot2d


    subroutine plot3d(x, y, z, options, key)
    ! Plot a 3D figure from x, y, and z arrays
        
        real, intent(in), dimension(:) :: x, y, z
        character(len=*), intent(in), optional :: options, key

        integer :: xn, yn, zn, i, j
        character(6)   :: plot_cmd
        character(:), allocatable :: datafilepath

        ! Initialize values
        plot_cmd = 'splot'

        ! Find the array sizes of the two arrays
        xn = size(x)
        yn = size(y)
        zn = size(z)

        ! Check whether z array is of the right size or not
        if ((xn /= yn) .and. (yn /= zn)) then
            stop "Length of x, y and z must be same."
        end if

        DATAFID = DATAFID + 1
        
        ! Create the data filename path
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT

        ! Write the arrays to data file
        open(DATAFID, file=datafilepath)
        write(DATAFID,*) "# x   y   z"
        write(DATAFID,*) (x(i), y(i), z(i), NEW_LINE('C'), i=1, xn)
        close(DATAFID)

        call plotfile(plot = plot_cmd, datafile = trim(datafilepath), &
                      options = trim(options), key = trim(key))

    end subroutine plot3d


    subroutine plot3dg(x, y, z, options, key)
    ! Plot 3D figures. z is 2D array. Hence the z data is already 
    ! in grid format.

        real, intent(in), dimension(:) :: x, y
        real, intent(in), dimension(:,:) :: z
        character(len=*), intent(in), optional :: options, key

        integer :: xn, yn, zn, i, j
        character(6)   :: plot_cmd
        character(:), allocatable  :: datafilepath

        ! Initialize values
        plot_cmd = 'splot'

        call datafile_xyz_grid(x,y,z,datafilepath)

!         ! Find the array sizes of the two arrays
!         xn = size(x)
!         yn = size(y)
!         zn = size(z)
! 
!         ! Check whether z array is of the right size or not
!         if (zn /= xn*yn) then
!             stop "Supplied size(z) /= size(x) * size(y)"
!         end if
! 
!         DATAFID = DATAFID + 1
!         
!         ! Create the data filename path
!         datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT
! 
!         ! Write the arrays to data file
!         open(DATAFID, file=datafilepath)
!         write(DATAFID,*) "# x   y   z"
!         do i = 1,xn
!             write(DATAFID,*) (x(i), y(j), z(i,j), NEW_LINE('C'), j=1,yn)
!         end do
!         close(DATAFID)

        call plotfile(plot = plot_cmd, datafile = trim(datafilepath), &
                      options = trim(options), key = trim(key))

    end subroutine plot3dg

    ! See Normalization under
    ! https://in.mathworks.com/help/matlab/ref/matlab.graphics.chart.primitive.histogram-properties.html
    ! https://stackoverflow.com/questions/2471884/histogram-using-gnuplot
    ! Draw histogram from data array 
    subroutine hist(x,nbins,norm,key,fillcolor,linecolor,alpha)
    ! Draw a histogram

        real, dimension(:), intent(in) :: x
        integer, intent(in), optional :: nbins
        character(len=*), intent(in), optional :: norm, key, &
            fillcolor, linecolor
        real, intent(in), optional :: alpha

        character(:), allocatable :: datafilepath

        call hist_common1(x,datafilepath)

        call histfile(datafilepath,nbins,norm,key, &
                    fillcolor,linecolor,alpha)

    end subroutine hist


    subroutine histfile(datafilepath,nbins,norm,key, &
                    fillcolor,linecolor,alpha)
    ! Draw histogram from file 

        character(len=*), intent(in) :: datafilepath
        integer, intent(in), optional :: nbins
        character(len=*), intent(in), optional :: norm, key, &
            fillcolor, linecolor
        real, intent(in), optional :: alpha

        character(20) :: fillcolor_final, linecolor_final
        real :: alpha_final 
        integer :: records, j
        character(:), allocatable :: hist_config

        ! Initialize values
!         alpha_final = 0.6       ;   fillcolor_final = 'green' ;
!         linecolor_final = 'green'  ;  
!         
!         ! Update the user values
!         if (present(alpha)) alpha_final = alpha
!         if (present(fillcolor)) fillcolor_final  = trim(fillcolor)
!         if (present(linecolor)) linecolor_final  = trim(linecolor)
! 
        call hist_common2(datafilepath,nbins,norm)
        
        alpha_final = get_real(0.6,alpha)
        fillcolor_final = get_character('green',fillcolor)
        linecolor_final = get_character('seagreen',linecolor)

        ! http://gnuplot.sourceforge.net/demo/fillstyle.html
        call gpcmd('set style fill solid ' // real2str(alpha_final))

        hist_config = 'using (hist($1,dx)):(norm_factor)' & 
            // ' smooth frequency with boxes' & 
            // ' fillcolor "' // trim(fillcolor_final) // '"' &
            // ' fillstyle solid border linecolor' &
            // ' "' // trim(linecolor_final) // '"'

        call plotfile(plot = 'plot', datafile = trim(datafilepath), &
            options = trim(hist_config), key = key)

    end subroutine histfile


    subroutine histtable(x,outfile,nbins,norm)
    ! Create histogram table
    
        real, dimension(:), intent(in) :: x
        character(len=*), intent(in) :: outfile
        integer, intent(in), optional :: nbins
        character(len=*), intent(in), optional :: norm

        character(:), allocatable :: datafilepath, hist_config

        call hist_common1(x,datafilepath)
        call hist_common2(datafilepath,nbins,norm)

        hist_config = 'using (hist($1,dx)):(norm_factor)' & 
            // ' smooth frequency with boxes' 

        call gpcmd("set table '" // trim(outfile) // "'")
        call gpcmd("plot '" // trim(datafilepath) // "' " // trim(hist_config))
        call gpcmd("unset table")    

        call standby(trim(outfile),0.2,10)

    end subroutine histtable


    subroutine hist_common1(x,datafilepath)
    ! Histogram common commands 1

        real, dimension(:), intent(in) :: x
        integer :: xn, j
        character(:), allocatable :: datafilepath        

        ! Get the size of the data: no. of data points 
        xn = size(x)

        DATAFID = DATAFID + 1

        ! Create the data filename path
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT

        open(DATAFID,file = trim(datafilepath))
        write(DATAFID,*) (x(j),NEW_LINE('C'), j = 1, xn)
        close(DATAFID)

    end subroutine hist_common1


    subroutine hist_common2(datafilepath,nbins,norm)
    ! Histogram common commands 2

        character(len=*), intent(in) :: datafilepath
        integer, intent(in), optional :: nbins
        character(len=*), intent(in), optional :: norm

        integer :: nbins_final
        character(20) :: norm_final

        ! Initialize values
!         nbins_final = 50        ;  norm_final = 'count' 
!         ! Update the user values
!         if (present(nbins)) nbins_final = nbins
!         if (present(norm))  norm_final  = trim(norm)

        nbins_final = get_integer(50,nbins)
        norm_final = get_character('count',norm)

        ! Get statistical report
        call gpcmd("stats '" // trim(datafilepath) // "' using 1 nooutput")

        ! Get the STATS details
        call gpcmd("records = STATS_records")
        call gpcmd("xmin = STATS_min")
        call gpcmd("xmax = STATS_max")
        call gpcmd("nbins = " // int2str(nbins_final))
        call gpcmd("dx = (xmax - xmin) / real(nbins)")

        ! Get norm_factor
        if ( trim(norm_final) == 'count' ) then
            call gpcmd("norm_factor = 1.0") 
        elseif ( trim(norm_final) == 'countdensity' ) then
            call gpcmd("norm_factor = 1.0/dx")
        elseif ( trim(norm_final) == 'probability' ) then
            call gpcmd("norm_factor = 1.0 / records")
        elseif ( trim(norm_final) == 'pdf' ) then
            call gpcmd("norm_factor = 1.0 /(records * dx)")
        else
            write(*,*) "Normalization '", trim(norm_final), "' not supported."
            write(*,*) "Aborting."
            stop 
        end if

        call gpcmd('hist(x,dx) = dx*( floor( (x-xmin)/dx ) + 0.5 ) + xmin')
        call gpcmd('set boxwidth dx*0.9')

    end subroutine hist_common2


    function get_fwhm(gaussian,infile,outfile,ivar) result(fwhm)
    ! Get Full-Width-Half-Maximum of a histogram

        character(len=*), intent(in) :: gaussian
        character(len=*), intent(in), optional :: infile, outfile, ivar
        real :: fwhm
        
        character(:), allocatable :: datafilepath, fortfile, ivar_final

        ! Fix indepdendent variable
!         ivar_final = 'x'
!         if(present(ivar)) ivar_final = trim(ivar)
        ivar_final = get_character('x', ivar)
        call gpcmd("set dummy " // trim(ivar_final))

        ! Load the input parameter file if provided
        if (present(infile)) then
            call gpcmd("load '" // trim(infile) // "'")
        end if

        ! Name of datafile
        DATAFID = DATAFID + 1
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT

        ! Create the table
        call gpcmd("set table '" // trim(datafilepath) // "'")
        call gpcmd("gaussian("// trim(ivar_final) // ") = " // gaussian)
        call gpcmd("plot " // trim(gaussian) // " with lines")
        call gpcmd("unset table")
   
        ! Load the ignoreu function
        call ignoreu()        
        
        ! Get some basic statistics
        call gpcmd("stats '" // trim(datafilepath) // &
                    "' using ($1):(ignoreu($2,3)) nooutput")
        call gpcmd("ymax = STATS_max_y")

        call gpcmd("half_ymax = ymax / 2.0")
        call gpcmd("yep = ymax * 0.2")

        ! Define the roots function
        call gpcmd("roots("// trim(ivar_final) //") = ((" // &
                    trim(gaussian) // " - half_ymax) > 0)" &
                    // " ? half_ymax : 1/0")        
        
        ! Create another datafile
        DATAFID = DATAFID + 1
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT

        ! Set another table to find the root
        call gpcmd("set table '" // trim(datafilepath) // "'")
        call gpcmd("set yrange[0:ymax+yep]")
        call gpcmd("plot roots("// trim(ivar_final) //") with lines")
        call gpcmd("unset yrange")
        call gpcmd("unset table")

        ! Get the statistics again
        call gpcmd("stats '" // trim(datafilepath) // &
                     "' using ($1):(ignoreu($2,3)) nooutput")        

        ! Find the FWHM
        call gpcmd("fwhm_xmin = STATS_pos_min_y")
        call gpcmd("fwhm_xmax = STATS_pos_max_y")
        call gpcmd("fwhm = fwhm_xmax - fwhm_xmin")

        ! Write to value of FWHM to a fortran readable file
        FORTFID = FORTFID + 1
        fortfile = GPDIR // SEP // "fort_" // int2str(FORTFID,'i0.6') &
                        // '.' // FEXT
        call gpcmd("set print '" // trim(fortfile) // "'")
        call gpcmd("print fwhm")
        call gpcmd("unset print")

        ! Wait until the file is ready to be opened
        call standby(fortfile, 0.2, 15)

        ! Read the file and get FWHM 
        open(FORTFID, file = fortfile)
        read(FORTFID,*) fwhm
        close(FORTFID)

        ! If outfile present then write the FWHM data to file
        if (present(outfile)) then
            call gpcmd("set print '" // trim(outfile) // "'")
            call gpcmd("print '# FWHM data'")
            call gpcmd("print fwhm_xmin, ' 0'")
            call gpcmd("print fwhm_xmin, half_ymax")
            call gpcmd("print fwhm_xmax, half_ymax")
            call gpcmd("print fwhm_xmax, ' 0'")
            call gpcmd("unset print")        
        end if

    end function get_fwhm


    function get_plot_cmd(in_plot_cmd) result(out_plot_cmd)
    ! Decide whether to 'plot/splot' or ',' (if on hold)

        character(len=*), intent(in) :: in_plot_cmd
        character(:), allocatable :: out_plot_cmd

        ! If hold is on then 
        if (HOLD_ON) then
            ! Is it the first plot, then plot normally,
            ! using plot / splot command
            if (HOLD_COUNT == 1) then
                out_plot_cmd = trim(in_plot_cmd)
            else
            ! If the hold count is not 1 then, don't use plot/splot 
            ! command. Add to the previous plot.
                out_plot_cmd = ", \" // new_line('c')
            end if

            ! Increase hold counter
            HOLD_COUNT = HOLD_COUNT + 1

        else
            ! If not on hold use the plot / splot command
            out_plot_cmd = trim(in_plot_cmd)
        end if        

    end function get_plot_cmd


    subroutine fplot2d(func,xmin,xmax,xn,options,key)
    ! Plot a 2D function

        real :: func
        real, intent(in) :: xmin, xmax
        integer, intent(in), optional :: xn
        character(len=*), intent(in), optional :: options, key

        real, dimension(:), allocatable :: x, y
        integer :: i, ok, xn_final
        real :: dx
        character(:), allocatable :: plot_cmd, datafilepath

        if (xmin >= xmax) stop "xmin should be less than xmax. Aborting."

        ! Initialize values
        plot_cmd = 'plot'

!         xn_final = 100   ! Default value
!         if (present(xn)) xn_final = xn
        xn_final = get_integer(100, xn)

        allocate(x(xn_final), y(xn_final), stat = ok)
        if (ok /= 0) stop "Couldn't allocate x, y"

        dx = (xmax - xmin) / (real(xn_final) - 1)
        x = [(xmin + (i-1)*dx, i = 1,xn_final)]

        do i = 1, xn_final
            y(i) = func(x(i))
        end do

!         print*,(x(i),y(i), new_line('c'), i = 1,xn_final)
        DATAFID = DATAFID + 1
        
        ! Create the data filename path
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT

        open(DATAFID, file=trim(datafilepath))
        write(DATAFID,*) "# x   y"
        write(DATAFID,*) (x(i),y(i),NEW_LINE('C'),i=1,xn_final)
        close(DATAFID)

        call plotfile(plot = plot_cmd, datafile = trim(datafilepath), &
                      options = trim(options), key = trim(key))
        
    end subroutine fplot2d


    subroutine fplot3d(func,xmin,xmax,ymin,ymax,xn,yn,options,key)
    ! Plot a 3D function

        real :: func
        real, intent(in) :: xmin, xmax, ymin, ymax
        integer, intent(in), optional :: xn, yn
        character(len=*), intent(in), optional :: options, key

        real, dimension(:), allocatable :: x, y
        real, dimension(:,:), allocatable :: z
        integer :: i, j, xn_final, yn_final, ok
        real :: dx, dy
        character(:), allocatable :: plot_cmd, datafilepath

        if (xmin >= xmax) stop "xmin should be less than xmax. Aborting."
        if (ymin >= ymax) stop "ymin should be less than ymax. Aborting."

        ! Initialize values
        plot_cmd = 'splot'

!         xn_final = 100   ! Default value
!         if (present(xn)) xn_final = xn
! 
!         yn_final = 100   ! Default value
!         if (present(yn)) yn_final = yn
        
        xn_final = get_integer(100,xn)
        yn_final = get_integer(100,yn)

        allocate(x(xn_final), y(yn_final), z(xn_final,yn_final), stat = ok)
        if (ok /= 0) stop "Couldn't allocate x, y, or z"

        dx = (xmax - xmin) / (real(xn_final) - 1)
        x = [(xmin + (i-1)*dx, i = 1,xn_final)]
        
        dy = (ymax - ymin) / (real(yn_final) - 1)
        y = [(ymin + (j-1)*dy, j = 1,yn_final)]

        do i = 1, xn_final
            do j = 1, yn_final
                z(i,j) = func(x(i),y(j))
            end do
        end do

        DATAFID = DATAFID + 1
        
        ! Create the data filename path
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT

        open(DATAFID, file=trim(datafilepath))
        write(DATAFID,*) "# x   y   z"
        do i = 1, xn_final
            write(DATAFID,*) (x(i),y(j),z(i,j),NEW_LINE('C'), j=1,yn_final)
        end do
        close(DATAFID)

        call plotfile(plot = plot_cmd, datafile = trim(datafilepath), &
                      options = trim(options), key = trim(key))
        
    end subroutine fplot3d


    subroutine polarplot(theta,rho,options,key,polar_options)
    ! Polar plot where theta is in radians

        real, dimension(:), intent(in) :: theta, rho
        character(len=*), intent(in), optional :: options, key, polar_options 

        call set_polarplot(.true.)

        if (present(polar_options)) call gpcmd(polar_options)

        call plot2d(theta,rho, options = options, key = key)
        
        call set_polarplot(.false.)

    end subroutine polarplot


    subroutine set_polarplot(setpolar)
    ! Subroutine to (un)set the polar configuration for the plot

        logical, intent(in) :: setpolar

        if (setpolar) then
            ! Set some modifications such that the graph looks nice.
            call gpcmd("set polar")
            call gpcmd("set size square")    
            call gpcmd("unset border")
            call gpcmd("unset xtics")
            call gpcmd("unset ytics")
            call gpcmd("unset raxis")
 
            ! Redefine a new line style for the grid
            call gpcmd("set style line 10 linetype 1 linecolor rgb 'gray'" &
                        // " linewidth 0.3")
            call gpcmd("set grid polar 30")
            call gpcmd("set grid linestyle 10")
        
            ! Set the theta labels
            call gpcmd("set ttics 0,30")

            ! Set the key position
            call gpcmd("set key rmargin")

        else

            ! Reset the polar config back
            call gpcmd("unset polar")
            call gpcmd("set size nosquare")    
            call gpcmd("set border")
            call gpcmd("set xtics")
            call gpcmd("set ytics")
 
            ! Redefine a new line style for the grid
            call gpcmd("unset style line 10")
            call gpcmd("unset grid")
        
            ! Set the theta labels
            call gpcmd("unset ttics")

            ! Set the key position
            call gpcmd("set key inside")

        end if

    end subroutine set_polarplot
    

    ! Elemental argument helps in passing arrays too
    elemental function deg2rad(theta_deg) result(theta_rad)
    ! Convert degree to radian
    
        real, intent(in) :: theta_deg
        real :: theta_rad, pi

        pi = 4.0 * atan(1.0)

        theta_rad = (pi/180.0) * theta_deg

    end function deg2rad


    ! Elemental argument helps in passing arrays too
    elemental function rad2deg(theta_rad) result(theta_deg)
    ! Convert radian to degree
    
        real, intent(in) :: theta_rad
        real :: theta_deg, pi

        pi = 4.0 * atan(1.0)

        theta_deg = (180.0/pi) * theta_rad

    end function rad2deg


    subroutine datafile_xyz_grid(x,y,z,datafilepath)

        real, intent(in), dimension(:) :: x, y
        real, intent(in), dimension(:,:) :: z
        
        integer :: xn, yn, zn, i, j
        character(:), allocatable :: datafilepath

        ! Find the array sizes of the two arrays
        xn = size(x)
        yn = size(y)
        zn = size(z)

        ! Check whether z array is of the right size or not
        if (zn /= xn*yn) then
            stop "Supplied size(z) /= size(x) * size(y)"
        end if

        DATAFID = DATAFID + 1
        
        ! Create the data filename path
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT

        ! Write the arrays to data file
        open(DATAFID, file=datafilepath)
        write(DATAFID,*) "# x   y   z"
        do i = 1,xn
            write(DATAFID,*) (x(i), y(j), z(i,j), NEW_LINE('C'), j=1,yn)
        end do
        close(DATAFID)

    end subroutine datafile_xyz_grid


    subroutine contour(x,y,z,clabel,filled,cntr_options,key)
        
        real, intent(in), dimension(:) :: x, y
        real, intent(in), dimension(:,:) :: z
        logical, intent(in), optional :: clabel, filled
        character(len=*), intent(in), optional :: cntr_options, key
        
        logical :: clabel_final, filled_final
        character(:), allocatable :: datafilepath, plot_options

        ! Get the optional logical values
        clabel_final = get_logical(.true.,clabel)
        filled_final = get_logical(.false.,filled)

        call datafile_xyz_grid(x,y,z,datafilepath)

        call gpcmd("set contour base")
!         call gpcmd("set cntrparam levels incremental 0, 10, 100")

        if (filled_final) then
            ! For filled contour
            call gpcmd("set cntrlabel onecolor")
            call gpcmd("set pm3d map")
            
            plot_options = 'with pm3d'
        else 
            ! For unfilled contour
            call gpcmd("set cntrlabel")
            call gpcmd("set view map")
            call gpcmd("unset surface")
!             call gpcmd("set nokey")

            plot_options = 'with lines'
        end if

        if (present(cntr_options)) call gpcmd(cntr_options)

        call hold(.true.)
        call plotfile(plot = 'splot', datafile = datafilepath, &
                        options = plot_options)
        
        ! Draw labels (if required)
        if (clabel_final) then
            call plotfile(plot = 'splot', datafile = datafilepath, &
                        options = 'with labels', key = key)
        end if
        
        call hold(.false.)

        ! Reset the contour settings
        if (filled_final) then
            ! Do nothing
            continue
        else 
            call gpcmd("set view 60,30,1,1") ! Default view
            call gpcmd("set surface")
            call gpcmd("set key")
        end if

    end subroutine contour


    subroutine plotfile(datafile, plot, options, key)
    ! Plot from a file

        character(len=*), intent(in) :: datafile
        character(len=*), intent(in), optional :: plot, options, key
        character(10) :: plot_cmd, plot_final
        character(300) :: gpstr

!         plot_cmd = 'plot'
!         if(present(plot)) plot_cmd = trim(plot)
        plot_cmd = get_character('plot', plot)

        ! Wait for the file to exist
        call standby(datafile,0.2,10)

        ! Decide whether to 'plot/splot' or 'replot'
        plot_final = get_plot_cmd(plot_cmd)
        
        ! Create the plot command string
        gpstr = trim(plot_final) // " '" // trim(datafile) // "'" 
        if (present(options)) then
            gpstr = trim(gpstr) // " " // trim(options)
        end if
        
        ! Add key (legend) if provided  
        if (present(key)) then
            gpstr = trim(gpstr) // ' title "' // trim(key) // '"'
        else
            gpstr =  trim(gpstr) // " notitle"            
        end if

        if (HOLD_ON) then
            ! If hold is on, then print to file.
            ! Later when hold is off, this file will be loaded.
            call print2file(trim(gpstr),HFID,.false.)
        else
            ! Send to gnuplot to plot
            call gpcmd(trim(gpstr))
        end if

    end subroutine plotfile


    subroutine cplot(cfunc,plot,infile,options,key,ivar)
    ! Plot a function (written as characters)

        character(len=*), intent(in) :: cfunc
        character(len=*), intent(in), optional :: plot, infile, options, &
            key, ivar

        character(6) :: plot_cmd, plot_final
        character(300) :: gpstr

!         plot_cmd = 'plot'
!         if(present(plot)) plot_cmd = trim(plot)
        plot_cmd = get_character('plot', plot)

        ! If the independent variable is changed from 'x'
        if (present(ivar)) then
            call gpcmd("set dummy " // trim(ivar))
        end if

        ! If input parameters file is present, then load it.
        if(present(infile)) then
            call gpcmd("load '" // trim(infile) // "'")
        end if

        ! Decide whether to 'plot/splot' or 'replot'
        plot_final = get_plot_cmd(plot_cmd)

        ! Add options to the gnuplot command
        gpstr = trim(plot_final) // " " // trim(cfunc) 
        if (present(options)) then
            gpstr = trim(gpstr) // " " // trim(options)
        end if

        ! Add key (legend) if provided  
        if (present(key)) then
            gpstr = trim(gpstr) // ' title "' // trim(key) // '"'
        else
            gpstr =  trim(gpstr) // " notitle"
        end if

        if (HOLD_ON) then
            ! If hold is on, then print to file.
            ! Later when hold is off, this file will be loaded.
            call print2file(trim(gpstr),HFID,.false.)
        else
            ! Send to gnuplot to plot
            call gpcmd(trim(gpstr))
        end if

    end subroutine cplot


    subroutine fit(x, y, func, infile, outfile, ivar, options)
    ! Fit a curve to x and y data points

        real, intent(in), dimension(:) :: x, y        
        character(len=*), intent(in) :: func, infile
        character(len=*), intent(in), optional :: outfile, ivar, options
        
        character(:), allocatable :: gpstr
        character(:), allocatable :: datafilepath
        integer :: j, xn, yn

        ! Find the array sizes of the two arrays
        xn = size(x) ;  yn = size(y)

        ! Check whether arrays match in size or not
        if (xn /= yn) then
            stop "Supplied arrays do not match in size"
        end if

        ! Create the datafile name
        DATAFID = DATAFID + 1
        datafilepath = GPDIR // SEP // int2str(DATAFID,'i0.6') // '.' // DEXT
        
        ! Write the arrays to data file
        open(DATAFID, file = trim(datafilepath))
        write(DATAFID,*) "# x   y"
        write(DATAFID,*) (x(j),y(j),new_line('c'),j=1,xn)
        close(DATAFID)

        call fitfile(datafilepath, func, infile, &
                    outfile, ivar, options)

    end subroutine fit


    subroutine fitfile(datafile, func, infile, &
                        outfile, ivar, fitoptions, options)

        character(len=*), intent(in) :: datafile        
        character(len=*), intent(in) :: func, infile
        character(len=*), intent(in), optional ::  outfile, ivar, &
                                        fitoptions, options
        
        character(:), allocatable :: gpstr, options_final
        integer :: j, xn, yn, stat

        ! Change the idependent variable.
        if (present(ivar)) then
            call gpcmd("set dummy " // trim(ivar))
        end if

        ! Configure fit command
        gpstr = "set fit quiet"
        if (present(fitoptions)) then
            gpstr = trim(gpstr) // " " // trim(fitoptions)
        end if
        call gpcmd(gpstr)

!         options_final = 'using 1:2'
!         if(present(options)) options_final = trim(options)
        options_final = get_character('using 1:2', options)

        ! Use the fit command
        gpstr = "fit " // trim(func) // " '" // trim(datafile) // "'" &
                // " " // trim(options_final) &
                // " via '" // trim(infile) // "'"
        call gpcmd(gpstr)

        ! Save the fit parameters to terminal 
        call gpcmd("save fit '-'")

        ! If outfile is present the save the values to the output file.
        if (present(outfile)) then

            ! Save the fit parameters to outfile
            call gpcmd("save fit '" // trim(outfile) // "'")
            call gpcmd("unset fit")

            ! Standby to open a file until it exists
            call standby(outfile,0.2,10)

            ! Sleep and allow gnuplot to write to file
!             call slumber(0.5)
        end if

    end subroutine fitfile


    subroutine gptofort(iofile, nmlist)
    ! Convert a file created by gnuplot to be ready for upload in fortran
    ! via namelist. Also replaces '#' comment symbol by '!' symbol.

        character(len=*), intent(in) :: iofile, nmlist
        character(:), allocatable :: tmpfilepath
        character(300) :: line
        integer :: stat, ios, unt1, unt2
    
        ! Standby until the file exists
        call standby(iofile,0.2,10)

        FORTFID = FORTFID + 1
        unt1 = FORTFID
    
        open(unt1, file = trim(iofile), iostat = stat)
        if (stat /= 0 ) stop "Cannot open fit parameter file."
!         print*, iofile

        FORTFID = FORTFID + 1
        unt2 = FORTFID

        ! Create temporary file
        tmpfilepath = GPDIR // SEP // "fit_" // int2str(FORTFID,'i0.6') &
                        // "." // FEXT

        open(unt2, file = trim(tmpfilepath), iostat = stat)
        if (stat /= 0 ) stop "Cannot open temporary file."

        ! Read the iofile and write to output file by replacing
        ! the gnuplot comment character '#' by '!'
        ! Also add the name-list group-id
        write(unt2,*) "&", trim(nmlist)
        do
            read(unt1, '(a)', iostat=ios) line
            if ( ios /= 0 ) exit

            ! Replace '#' character with '!'
            line = replace_text(line,'#','!')
    
            write(unt2,*) trim(line)
        end do
    
        ! End the namelist end 
        write(unt2,*) "&end"

        close(unt2)
        close(unt1)

        ! Rename the tmpfile
        call rename(tmpfilepath,iofile)
    
    end subroutine gptofort


    ! To set the title of the plot
    subroutine title(text)
        character(len=*), intent(in) :: text

        call set_parameter_text("title", text)

    end subroutine title
   

    subroutine xlabel(text)
    ! To set the xlabel of the plot
        
        character(len=*), intent(in) :: text
        
        call set_parameter_text("xlabel", text)

    end subroutine xlabel
    
    
    subroutine ylabel(text)
    ! To set the ylabel of the plot
        
        character(len=*), intent(in) :: text
        
        call set_parameter_text("ylabel", text)

    end subroutine ylabel


    subroutine zlabel(text)
    ! To set the zlabel of the plot
    
        character(len=*), intent(in) :: text

        call set_parameter_text("zlabel", text)

    end subroutine zlabel


    subroutine set_parameter_text(param,text)
    ! Set parameters with text options, like title, (x|y|z)label

        character(len=*), intent(in) :: param, text

        call gpcmd('set ' // trim(param) // ' "' // trim(text) // '"')

    end subroutine set_parameter_text


    ! To set the label of the plot
    subroutine label(tag,label_text,x,y,z,overlay,options)
        integer, intent(in) :: tag
        character(len=*), intent(in) :: label_text
        real, intent(in) :: x, y

        character(len=*), intent(in), optional :: overlay, options
        real, intent(in), optional :: z
        character(:), allocatable :: overlay_final
        
        character(30) :: coordinates
        character(:), allocatable :: label_cmd

        ! Get overlay, if provided, otherwise use default
!         overlay_final = 'screen'
!         if(present(overlay)) overlay_final = trim(overlay)
        overlay_final = get_character('screen', overlay)

        if (.not. present(z)) then
            ! For 2D plots
            write(coordinates,'(f4.2,a,f4.2,$)') x,',',y
        else
            ! For 3D plots
            write(coordinates,'(f4.2,a,f4.2,a,f4.2,$)') x,',',y,',',z
        end if

        ! For enhanced text including newline character: "\n"
        label_cmd = 'set label ' // int2str(tag,'i0') // &
            ' "' // trim(label_text) // '"' // &
            ' at ' // trim(overlay_final) // ' ' // &
            trim(coordinates) // ' ' // trim(options)

        call gpcmd(label_cmd)

    end subroutine label

    ! https://stackoverflow.com/questions/32523266/gnuplot-data-table-type-value-u-and-strange-bars-in-histogram-boxes
    subroutine ignoreu()
    ! Useful function to define to ignore the rows with 'u' values in the 
    ! column 'col' when the data is saved as a table by gnuplot.
    ! This is due to a bug in gnuplot
    ! https://sourceforge.net/p/gnuplot/bugs/1274/

        call gpcmd("ignoreu(x,col) = (x == 0 || strcol(col)" &
                    // " eq 'u' ? 1/0 : x)")

    end subroutine ignoreu


    ! https://stackoverflow.com/questions/22460220/character-returning-function-of-unknown-length
    function int2str(i,wfmt) result(string)
    ! Convert integer to string
    ! Input :
    !   i   : Mandatory, integer. Value to be converted to string
    !   wfmt: Optional, character. Format of integer
    ! Output:
    !   string: Output string.

        integer, intent(in) :: i
        character(len=*), intent(in), optional :: wfmt        
        character(:), allocatable :: string
        character(range(i)+2) :: tmp
        character(:), allocatable :: ffmt

        if (present(wfmt)) then
            ffmt = '(' // trim(wfmt) // ')'
            write(tmp,ffmt) i
        else
            write(tmp,'(i0)') i
        end if

        string = trim(tmp)

    end function int2str

    ! Convert real numbers to string
    function real2str(x,wfmt) result(string)

        real, intent(in) :: x
        character(len=*), intent(in), optional :: wfmt
        character(:), allocatable :: string
        character(range(x)+2) :: tmp
        character(:), allocatable :: ffmt
        
        if (present(wfmt)) then
            ffmt = '(' // trim(wfmt) // ')'
            write(tmp,ffmt) x
        else
            write(tmp,*) x
        end if

        string = trim(tmp)

    end function real2str

    subroutine xrange(llim,ulim)
    ! To set the xrange of the plot
    
        real, intent(in), optional :: llim, ulim

        call xyzrange("xrange", llim, ulim)

    end subroutine xrange


    subroutine yrange(llim,ulim)
    ! To set the yrange of the plot
    
        real, intent(in), optional :: llim, ulim

        call xyzrange("yrange", llim, ulim)

    end subroutine yrange


    subroutine zrange(llim,ulim)
    ! To set the zrange of the plot
    
        real, intent(in), optional :: llim, ulim

        call xyzrange("zrange", llim, ulim)

    end subroutine zrange


    subroutine xyzrange(param,llim,ulim)
    ! To set the (x|y|z)range of the plot
        
        character(len=*), intent(in) :: param
        real, intent(in), optional :: llim, ulim

        character(:), allocatable :: llim_final, ulim_final        
        character(:), allocatable :: range_cmd

!         llim_final = '' ;         ulim_final = '' ;
!         if (present(llim)) llim_final = real2str(llim)
!         if (present(ulim)) ulim_final = real2str(ulim)

        llim_final = get_character('',real2str(llim))
        ulim_final = get_character('',real2str(ulim))

        range_cmd = "set " // trim(param) // " [" // llim_final // &
                        ":" // ulim_final // "]"

        call gpcmd(range_cmd)
    end subroutine xyzrange


    ! To plot multiple graphs on the same plot window
    subroutine hold(hold_status)
        logical, intent(in) :: hold_status
        character(:), allocatable :: holdfile

        HOLD_ON = hold_status

        if (HOLD_ON) then

            ! Set the hold counter to one
            HOLD_COUNT = 1

            ! Increase the hold file id by one, create the filename
            ! and open the file
            HFID = HFID + 1
            holdfile = GPDIR // SEP // "hold_" // int2str(HFID,'i0.6') &
                            // "." // GEXT
            open(HFID, file = trim(holdfile))

        else
            ! Reset the counter to zero
            HOLD_COUNT = 0
            
            ! Re-create name of hold file
            holdfile = GPDIR // SEP // "hold_" // int2str(HFID,'i0.6') &
                            // "." // GEXT

            ! Print a newline character to the hold file and close it
            call print2file("",HFID,.true.)
            close(HFID)

            ! Wait for the hold file to exist
            call standby(trim(holdfile),0.2,10)

            ! Load the hold file
            call gpcmd("load '"// trim(holdfile) // "'")

        end if

    end


!     subroutine key(stat, options)
!     ! Unset/set key
!     
!         logical, intent(in) :: stat
!         character(len=*), intent(in), optional :: options
! 
!         call set_parameter("key",stat,options)
! 
!     end subroutine key

    subroutine key(options)
    ! Unset/set key
    
        character(len=*), intent(in) :: options

        call gpcmd("set key " // options)

    end subroutine key


    subroutine grid(stat, options)
    ! Unset/set grid
    
        logical, intent(in) :: stat
        character(len=*), intent(in), optional :: options

        call set_parameter("grid",stat,options)

    end subroutine grid


    subroutine multiplot(stat,options)
    ! Unset/set multiplot
    
        logical, intent(in) :: stat   
        character(len=*), intent(in), optional :: options

        call set_parameter("multiplot",stat,options)
        
    end subroutine multiplot


    subroutine set_parameter(param,stat,options)
    ! Unset/set parameters, like key, grid and multiplot 
    
        character(len=*), intent(in) :: param        
        logical, intent(in) :: stat
        character(len=*), intent(in), optional :: options

        if (stat) then
            if (present(options)) then
                call gpcmd("set " // trim(param) // " " // trim(options))
            else 
                call gpcmd("set " // trim(param))
            end if
        else
            call gpcmd("unset " // trim(param))
        end if        

    end subroutine set_parameter


    ! Open a pipe to gnuplot via C
    subroutine openplot(closeall,debug,delete)
        logical, intent(in), optional :: closeall, debug
        logical :: closeall_final, debug_final
        logical, intent(in), optional :: delete
        logical :: delete_final 
        character(len=5), dimension(3) :: ext
        integer :: k, next, ok

        closeall_final = get_logical(.true., closeall)
        delete_final = get_logical(.true., delete)

        ! File extensions to be deleted
        ext(1) = GEXT ;   ext(2) = FEXT
        ext(3) = DEXT ;

        next = size(ext)

        ! Remove the temporary files
        if (delete_final) then

            do k = 1, next
                ! Delete all temporary files in current directory
                call c_delete_ext_files("."//C_NULL_CHAR, &
                                    trim(ext(k))//C_NULL_CHAR) ;
                
                ! Delete all temporary files in GPDIR directory
                call c_delete_ext_files(GPDIR//C_NULL_CHAR, &
                                    trim(ext(k))//C_NULL_CHAR) ;

            end do

            ! Remove the empty directory 
            call rmdir(trim(GPDIR)) 
        end if

        if (closeall_final) then
            ! Kill any previous gnuplot windows
            call c_kill_gnuplot()

            ! Wait before calling next gnuplot 
            call slumber(0.1)
        end if

        debug_final = get_logical(.false., debug)

        DEBUG_ON = debug_final

        call c_open_gp()
        call mkdir(GPDIR)

    end subroutine openplot


    subroutine test()
        call gpcmd("test")
    end subroutine test


    ! Close gnuplot pipe via C
    ! Also delete temporary files
    subroutine closeplot(delay)
        
        real, intent(in), optional :: delay

        real :: delay_final 
        logical :: fileopen 
        integer :: k
        character(:), allocatable :: gpfile

        delay_final = get_real(0.0, delay)

        ! Close gnuplot 
        call c_close_gp()

        ! Wait for delay_final secs before closing the open figure files
        call slumber(delay_final)

        ! Close all the open files
        do k = FIGID_INITIAL, FIGID_FINAL
            gpfile = GPDIR // SEP // "fig_" // int2str(k,'i0.6') &
                        // '.' // GEXT 
            inquire(file = gpfile, opened = fileopen)
            if (fileopen) then
                close(k)
            end if
        end do

    end subroutine closeplot


    ! Print text to file, with/without newline
    subroutine print2file(text,pid,newline)
        character(len=*), intent(in) :: text
        integer, intent(in) :: pid
        logical, optional, intent(in) :: newline

        ! Write to file
        if (newline) then 
            ! If newline is required
            write(pid,*) trim(text)
        else
            ! If newline is NOT desired at the end
            write(pid,'(a,$)') trim(text)
        end if        

    end subroutine print2file


    subroutine gpcmd(text,newline,tofile)
    ! Print to terminal and/or to fig files 

        character(len=*), intent(in)  :: text
        logical, intent(in), optional :: newline, tofile
        character(:), allocatable :: errmsg, gpfile
        logical :: newline_final, tofile_final, fileopen
        integer :: stat

        ! Initialize newline
!         newline_final = .true.
!         if (present(newline)) newline_final = newline
! 
!         tofile_final = .true.
!         if (present(tofile)) tofile_final = tofile

        newline_final = get_logical(.true., newline)
        tofile_final = get_logical(.true., tofile)

        ! Pass to c print to print to gnuplot
        call c_print(trim(text)//C_NULL_CHAR, &
                         newline_final, DEBUG_ON)

        if (tofile_final) then
            ! Print to gpfile 
            gpfile = GPDIR // SEP // "fig_" // int2str(FIGID_FINAL,'i0.6') &
                        // '.' // GEXT 
            inquire(file = gpfile, opened = fileopen)
            if (.not. fileopen) then
                open(FIGID_FINAL, file = gpfile, iostat = stat)
                if (stat /= 0) &
                    stop "Couldn't open the gnuplot file for writing."
            end if        
            call print2file(text,FIGID_FINAL,newline_final)

        end if

    end subroutine gpcmd


    ! Save the output as:
    subroutine saveas(savefile, savefig, options, driver)
        character(len=*), intent(in) :: savefile
        character(len=*), intent(in), optional :: options, driver
        integer, intent(in), optional :: savefig

        character(:), allocatable :: ext, set_term, errmsg, gpfile, &
                                     driver_final, filename, epsfile
        integer :: savefig_final
        logical :: fileopen

        savefig_final = get_integer(FIGID_FINAL, savefig)

        ext = lower(extname(savefile))

        if (ext == 'eps') then

            driver_final = get_character('epscairo',driver)

            if (driver_final == 'epscairo') then
                set_term = "set terminal epscairo" // &
                    "  enhanced color notransparent"

            elseif ((driver_final == 'postscript') .or. &
                    (driver_final == 'ps')) then
                set_term = "set terminal postscript" // &
                    " enhanced color eps"

            else
                call no_driver_support(ext,driver_final)
            end if

        elseif (ext == 'pdf') then

            driver_final = get_character('pdfcairo',driver)

            if (driver_final == 'pdfcairo') then
                set_term = "set terminal pdfcairo" // &
                    " enhanced color notransparent"
            
            elseif ((driver_final == 'postscript') .or. &
                    (driver_final == 'ps')) then
                
                ! Check if required software is present or not
                call c_check_software('epstopdf'//C_NULL_CHAR)
                
                set_term = "set terminal postscript" // &
                    " enhanced color eps"
            
            else 
                call no_driver_support(ext,driver_final)
            end if

        elseif (ext == 'tex') then
            
            driver_final = get_character('cairolatex',driver)

            if (driver_final == 'cairolatex') then
                set_term = "set terminal cairolatex" // &
                    " color pdf notransparent"
            
            elseif (driver_final == 'epslatex') then    
                set_term = "set terminal epslatex" // &
                    " color background 'white'"
            
            else
                call no_driver_support(ext,driver_final)
            end if
        
        elseif (ext == 'ps') then
            set_term = "set terminal postscript" // &
                " enhanced color"

        elseif (ext == 'png') then
            set_term = "set terminal pngcairo" // &
                " enhanced color notransparent"

        elseif ((ext == 'jpg') .or. (ext == 'jpeg')) then
            set_term = "set terminal jpeg" // &
                " enhanced"

        elseif (ext == 'svg') then
            set_term = "set terminal svg" // &
                " enhanced"

        elseif (ext == 'gif') then
            set_term = "set terminal gif" // &
                " enhanced"

        else
            print*, "Extension '" // trim(ext) // "' not supported for saving."
            return

        end if

        ! Reset before saving to file
        call gpcmd("reset", tofile = .false.)

        ! Append save as options if supplied 
        if (present(options)) then
            call gpcmd(trim(set_term) // ' ' // trim(options), tofile = .false.)
        else
            call gpcmd(trim(set_term), tofile = .false.)
        end if

        ! If output is pdf and driver is 'postscript' then treat separately
        if ((ext == 'pdf' .and. driver_final == 'postscript') .or. &
            (ext == 'pdf' .and. driver_final == 'ps')) then

            call gpcmd("set output '| epstopdf --filter -o " // &
                        trim(savefile) // "'", tofile = .false.)
        
!             ! For windows (Still doesn't work. However, works on linux)
!             ! ************
!             filename = get_basename(savefile)
!             epsfile  = filename // '.eps'
! !             print*, "epsfile = ", epsfile
! 
!             call gpcmd("set output '" // trim(epsfile) // "'", &
!                         tofile = .false.)
!             ! ************
                    
        else
            call gpcmd("set output '" // trim(savefile) // "'", &
                        tofile = .false.)
        end if

        ! Close the figure file
        gpfile = GPDIR // SEP // "fig_" // int2str(savefig_final,'i0.6') &
                    // '.' // GEXT 
        inquire(file = gpfile, opened = fileopen)
        if (fileopen) then
            close(savefig_final)
        end if

        call gpcmd("load '" // trim(gpfile) //"'", tofile = .false.)

        ! Need to reset the output and terminal
        call gpcmd("unset output", tofile = .false.)
        call gpcmd("unset terminal", tofile = .false.)

!         ! For windows (Still doesn't work. However, works on linux)
!         ! ************
!         ! If output is pdf and driver is 'postscript' then treat separately
!         if (ext == 'pdf' .and. driver_final == 'postscript') then
!             call standby(trim(epsfile),1.0,10)
!             call gpcmd("!epstopdf '" // trim(epsfile) // "'", &
!                         tofile = .false.)
!         end if
!         ! ************

        ! Standby until the output file has been created.
        call standby(trim(savefile),1.0,10)

    end subroutine saveas


    ! Show error message for no driver support and abort.
    subroutine no_driver_support(ext,driver)
        character(len=*), intent(in) :: ext, driver

        print*, "Driver '", trim(driver), "'", &
            " not supported for '", trim(ext), "' output. Aborting."
        stop 

    end subroutine no_driver_support


    ! Get logical value from default and optional 
    logical function get_logical(default_value, optional_value)
        logical, intent(in) :: default_value
        logical, intent(in), optional :: optional_value

        if (present(optional_value)) then
            get_logical = optional_value
        else
            get_logical = default_value
        end if

    end function get_logical


    ! Get real value from default and optional
    real function get_real(default_value,optional_value)
        real, intent(in) :: default_value
        real, intent(in), optional :: optional_value

        if (present(optional_value)) then
            get_real = optional_value
        else
            get_real = default_value
        end if

    end function get_real


    ! Get character value from default and optional
    function get_character(default_value,optional_value) 
        character(len=*), intent(in) :: default_value
        character(len=*), intent(in), optional :: optional_value
        character(:), allocatable :: get_character

        if (present(optional_value)) then
            get_character = trim(optional_value)
        else
            get_character = trim(default_value)
        end if

    end function get_character


    ! Get integer value from default and optional
    integer function get_integer(default_value,optional_value)
        integer, intent(in) :: default_value
        integer, intent(in), optional :: optional_value

        if (present(optional_value)) then
            get_integer = optional_value
        else
            get_integer = default_value
        end if

    end function get_integer


    function get_basename(infile) result(basename)
        character(len=*) :: infile
        character(:), allocatable :: basename
        integer :: idx

        idx = rindex(infile,'.')
        basename = trim(infile(:idx-1))

    end function get_basename


    ! Get the file extension of a file
    function extname(infile) result(ext)
        character(len=*) :: infile
        character(:), allocatable :: ext
        integer :: idx

        idx = rindex(infile,'.')
        ext = trim(infile(idx+1:))

    end function extname


    ! Index of last occurrence of 'substring' in 'string'
    function rindex(string,substring) result(i)
        character(len=*) :: string, substring
        character(len(string)) :: temp
        integer :: i, length, idx

        length = len(string)
        temp = reverse_string(string)
!         print*, "'",temp,"'"
        idx = index(temp,substring)
        i = length - idx + 1

    end function rindex


    ! Reverse the string
    function reverse_string(string) result(gnirts)
    
        character(len=*) :: string
        character(len(string)) :: gnirts
        integer :: i, length

        length = len(string) 

        do i = 1,length
            gnirts(length-i+1:length-i+1) = string(i:i)
        end do

    end function reverse_string

    ! http://fortranwiki.org/fortran/show/String_Functions
    function lower(s1)  result(s2)
        character(*)       :: s1
        character(len(s1)) :: s2
        character          :: ch
        integer, parameter :: duc = ichar('A') - ichar('a')
        integer            :: i

        do i = 1, len(s1)
            ch = s1(i:i)
            if (ch >= 'A' .and. ch <= 'Z') ch = char(ichar(ch)-duc)
            s2(i:i) = ch
        end do
    end function lower


    ! http://fortranwiki.org/fortran/show/String_Functions
    function upper(s1) result (s2)
    character(*)       :: s1
    character(len(s1)) :: s2
    character          :: ch
    integer, parameter :: duc = ichar('A') - ichar('a')
    integer            :: i

    do i = 1, len(s1)
        ch = s1(i:i)
        if (ch >= 'a' .and. ch <= 'z') ch = char(ichar(ch)+duc)
        s2(i:i) = ch
    end do

    end function upper


    ! http://fortranwiki.org/fortran/show/String_Functions
    function replace_text(s,text,rep)  result(output)
        character(*)        :: s,text,rep
        ! provide output with extra 100 char len
        character(len(s)+100) :: output   
        integer             :: i, nt, nr

        output = s ; nt = len_trim(text) ; nr = len_trim(rep)
        do
            i = index(output,text(:nt)) ; if (i == 0) exit
            output = output(:i-1) // rep(:nr) // output(i+nt:)
        end do
    end function replace_text

    
    ! Remove directory
    subroutine rmdir(dirname)
        character(len=*), intent(in) :: dirname

        call c_rmdir(trim(dirname)//C_NULL_CHAR)

    end subroutine rmdir


    subroutine mkdir(dirname)
    ! Create a directory 

        character(len=*), intent(in) :: dirname

        call c_mkdir(trim(dirname)//C_NULL_CHAR)

        call standby(trim(dirname), 0.2, 20, dir = .true.)

    end subroutine mkdir


    subroutine standby(portfolio, delay, maxiter, dir)
    ! Standby to open a file/folder until it exists

        character(len=*), intent(in) :: portfolio
        real, intent(in) :: delay
        integer, intent(in) :: maxiter
        logical, intent(in), optional :: dir

        integer :: iter, folio_exists, c_check_dir, c_check_file
        logical :: dir_final
        character(:), allocatable :: folio_type

!         dir_final = .false.
!         if (present(dir)) dir_final = dir
        dir_final = get_logical(.false., dir)

        iter = 0
        do
            ! Check if portfolio exists or not
            if (dir_final) then
                ! If directory
                folio_type = 'folder'
                folio_exists = c_check_dir(portfolio//C_NULL_CHAR)
            else
                ! If file
                folio_type = 'file'
                folio_exists = c_check_file(portfolio//C_NULL_CHAR)
            end if
             
!             print*, iter, trim(portfolio), ", ", trim(folio_type)
            
            ! If return value is 0 file/folder exists 
            ! otherwise file/folder does not exist.
            if (folio_exists == 0) exit

            if (iter >= maxiter) then
                print*, "Couldn't access the ", trim(folio_type), &
                        " '", trim(portfolio), "'. Aborting."
                stop 
            end if
            
            iter = iter + 1

            ! If file / folder does not exist then wait for a while
            ! and then check again.
!             print*, "Delay in fortran = ", delay
            call slumber(delay)

            ! Try to create the folder again
            if (dir_final) call c_mkdir(trim(portfolio)//C_NULL_CHAR)

        end do
    end subroutine standby


    ! Sleep in milliseconds
    subroutine slumber(delay) ! x = default real number of seconds to "sleep"
    real, intent(in):: delay

        call c_slumber(delay) 

    end subroutine slumber

!     ! Thanks to John Harper for the following subroutine from the site
!     ! http://objectmix.com/fortran/377490-pause-program-execution.html 
!     subroutine slumber(delay) ! x = default real number of seconds to "sleep"
!     real, intent(in):: delay
!     real:: cpu1,cpu2
!     call cpu_time(cpu1)
!     do
!         call cpu_time(cpu2)
!         if (cpu1 < 0. .or. cpu2 < 0.) then
!             print *,"This processor can't give a useful cpu_time :-("
!             return
!         end if
!         if (cpu2-cpu1 > delay) return
!     end do
!     end subroutine slumber

end module fortplot
