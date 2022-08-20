module kinds
    implicit none
    integer, parameter :: DP = kind(1.0d0)
    integer, parameter, public :: ucs4 = selected_char_kind('ISO_10646')
end module kinds

module ANSIColoursModule
    use kinds
    implicit none
    private

    type colours_type
        character(len=7) :: red = achar(27)//'[0;31m'
        character(len=7) :: green = achar(27)//'[0;32m'
        character(len=7) :: blue = achar(27)//'[0;34m'
        character(len=7) :: cyan = achar(27)//'[0;36m'
        character(len=7) :: magenta = achar(27)//'[0;35m'
        character(len=7) :: yellow = achar(27)//'[0;33m'
        character(len=7) :: black = achar(27)//'[0;30m'
        character(len=7) :: grey = achar(27)//'[0;90m'
        character(len=7) :: white = achar(27)//'[0;00m'
        character(len=7) :: reset = achar(27)//'[0;00m'
    end type colours_type

    type colours_unicode_type
        character(len=7,kind=ucs4) :: red = achar(27)//'[0;31m'
        character(len=7,kind=ucs4) :: green = achar(27)//'[0;32m'
        character(len=7,kind=ucs4) :: blue = achar(27)//'[0;34m'
        character(len=7,kind=ucs4) :: cyan = achar(27)//'[0;36m'
        character(len=7,kind=ucs4) :: magenta = achar(27)//'[0;35m'
        character(len=7,kind=ucs4) :: yellow = achar(27)//'[0;33m'
        character(len=7,kind=ucs4) :: black = achar(27)//'[0;30m'
        character(len=7,kind=ucs4) :: grey = achar(27)//'[0;90m'
        character(len=7,kind=ucs4) :: white = achar(27)//'[0;00m'
        character(len=7,kind=ucs4) :: reset = achar(27)//'[0;00m'
    end type colours_unicode_type

    type(colours_type), public :: colours
    type(colours_unicode_type), public :: colours_ucs4

    interface select_colour
        module procedure select_colour_default, select_colour_unicode
    end interface select_colour
    public :: select_colour

contains

    function select_colour_default(col) result(col_out)
        implicit none
        character(len=*), intent(in) :: col
        character(len=7) :: col_out

        select case (trim(col))
        case ('red')
            col_out = colours%red
        case ('green')
            col_out = colours%green
        case ('blue')
            col_out = colours%blue
        case ('cyan')
            col_out = colours%cyan
        case ('magenta')
            col_out = colours%magenta
        case ('yellow')
            col_out = colours%yellow
        case ('black')
            col_out = colours%black
        case ('grey')
            col_out = colours%grey
        case ('white')
            col_out = colours%white
        case default
            col_out = colours%reset
        end select
    end function select_colour_default

    function select_colour_unicode(col) result(col_out)
        implicit none
        character(len=*,kind=ucs4), intent(in) :: col
        character(len=7,kind=ucs4) :: col_out

        select case (trim(col))
        case (ucs4_'red')
            col_out = colours_ucs4%red
        case (ucs4_'green')
            col_out = colours_ucs4%green
        case (ucs4_'blue')
            col_out = colours_ucs4%blue
        case (ucs4_'cyan')
            col_out = colours_ucs4%cyan
        case (ucs4_'magenta')
            col_out = colours_ucs4%magenta
        case (ucs4_'yellow')
            col_out = colours_ucs4%yellow
        case (ucs4_'black')
            col_out = colours_ucs4%black
        case (ucs4_'grey')
            col_out = colours_ucs4%grey
        case (ucs4_'white')
            col_out = colours_ucs4%white
        case default
            col_out = colours_ucs4%reset
        end select
    end function select_colour_unicode

end module ANSIColoursModule


module x256ColoursModule
    use kinds
    implicit none

    type colourmap_type
        integer :: nColours = 1
        character(len=11,kind=ucs4), dimension(:), allocatable :: colourmap
    contains
        procedure :: init_colourmap
        procedure :: fill_colourmap
    end type colourmap_type

    type(colourmap_type) :: cmap_yellowred, cmap_blue, cmap_green
    type(colourmap_type) :: cmap_pink, cmap_cyanpurple

contains

    subroutine setup_colourmaps()
      implicit none

      integer :: nCols = 6

      call cmap_yellowred%init_colourmap(nCols)
      call cmap_blue%init_colourmap(nCols)
      call cmap_green%init_colourmap(nCols)
      call cmap_pink%init_colourmap(nCols)
      call cmap_cyanpurple%init_colourmap(nCols)

      call cmap_yellowred%fill_colourmap([226,220,214,208,202,196])
      call cmap_blue%fill_colourmap([51,45,39,33,27,21])
      call cmap_green%fill_colourmap([46,40,34,28,22,16])
      call cmap_pink%fill_colourmap([194,188,182,176,170,164])
      call cmap_cyanpurple%fill_colourmap([123,117,111,105,99,93])

    end subroutine setup_colourmaps


    function select_x256_colour(iColour) result(col_out)
        implicit none
        integer, intent(in) :: iColour
        character(len=11) :: col_out

        write(col_out,'(a7,i0.3,a1)') achar(27)//'[38;5;', iColour, 'm'
    end function  select_x256_colour


    subroutine init_colourmap(self, nCol)
        implicit none
        class(colourmap_type), intent(inout) :: self
        integer, intent(in) :: nCol

        if (self%nColours .ne. nCol) self%nColours = nCol
        if (allocated(self%colourmap)) deallocate(self%colourmap)
        allocate(self%colourmap(self%nColours))
    end subroutine init_colourmap


    subroutine fill_colourmap(self, colours_array)
        implicit none
        class(colourmap_type), intent(inout) :: self
        integer, dimension(:), intent(in) :: colours_array
        integer :: iCol, array_len, this_col

        array_len = size(colours_array)
        if (array_len .eq. self%nColours) then
            do iCol = 1, array_len
               this_col = colours_array(iCol)

               if (this_col.lt.0) then
                   self%colourmap(iCol) = select_x256_colour(0)
               else if (this_col.gt.256) then
                   self%colourmap(iCol) = select_x256_colour(256)
               else
                   self%colourmap(iCol) = select_x256_colour(colours_array(iCol))
               end if

            end do
        end if
    end subroutine fill_colourmap

end module x256ColoursModule



! ------------------------------------------------------------------
program mandelbrot
    ! writes a mandelbrot set to terminal, requires gfortran
    use, intrinsic :: iso_fortran_env, only: output_unit
    use kinds
    use ANSIColoursModule
    use x256ColoursModule
    implicit none

    real(DP) :: x, y, z_max

    real(DP) :: xmin = -2.0_DP
    real(DP) :: xmax =  1.0_DP
    real(DP) :: ymin = -1.0_DP
    real(DP) :: ymax =  1.0_DP

    complex(DP) :: z, c, z0
    integer :: iy, ix, n
    integer :: n_max
    integer :: n_print, n_print_max

    integer :: row, col
    character(len=128) :: temp_fileName

    logical :: print_divergence

    logical :: colour_ANSI
    type(colourmap_type) :: cmap

    character(kind=ucs4,len=16) :: colourChoice
    character(kind=ucs4,len=16) :: colour, reset
    character(kind=ucs4,len=16) :: solid_block

    integer iZoom

    call setup_colourmaps()
    call cmap%init_colourmap(cmap_blue%nColours)
    cmap = cmap_blue

    ! Make it so the default write supports unicode
    open(output_unit, encoding='utf-8')

    ! Solid block unicode character
    solid_block = char(int(z'2588'), kind=ucs4)

    print_divergence = .true.

    colourChoice = ucs4_'red'
    colour = select_colour(colourChoice)
    reset = colours_ucs4%reset

    ! get number of rows and columns in the terminal
    temp_fileName = 'temp_terminal_size.out'
    call execute_command_line('stty size >> ' // trim(temp_fileName))
    open(101, file=temp_fileName, action='read')
    read(101,*) row, col
    close(101)
    call execute_command_line('rm ' // trim(temp_fileName))
    row = row - 3 ! allow for bash prompt

    ! n_max = 4096
    z0 = 0.0_DP
    z_max = 2.0_DP

    iZoom = 0
    select case(iZoom)
    case(1)
        xmin = -0.7497_DP
        xmax = -0.7485_DP
        ymin = 0.115_DP
        ymax = 0.116_DP
        n_max = 1024
    case(2)
        n_max = 4096
        xmin = -0.235085_DP
        xmax = -0.235165_DP
        ymin = 0.827175_DP
        ymax = 0.827255_DP
    case default
        n_max = 256
    end select

    colour_ANSI = .false.
    n_print_max = int(exp(real(cmap%nColours+2,DP))+1.0_DP)
    n_print_max = int(log(real(n_print_max,DP))) - 1

    do iy = 1, row
       y = ymin + (ymax-ymin)/real(row-1,DP) * real(iy-1,DP)
       do ix = 1, col
          x = xmin + (xmax-xmin)/real(col-1,DP) * real(ix-1,DP)

          c = cmplx(x,y,DP)
          z = z0

          n = 0
          do
             n = n + 1
             call mandel(z, c)
             if (n.eq.n_max .or. abs(z).gt.z_max) exit
          end do

          if (print_divergence) then
              if (colour_ANSI) then
                  ! --------------------Display set in ANSI colours-------------------
                  n_print = int(log(real(n/8)))+1 ! scale divergence between 0 and 4
                  if (n_print.eq.4) then
                      ! make convergent points black
                      write(*, '(a)', advance='no') colours_ucs4%black &
                          & // trim(solid_block) // trim(colours_ucs4%reset)
                  else if (n_print.eq.2 .or. n_print.eq.3) then
                      ! otherwise colours according to divergence
                      write(*, '(a)', advance='no') colours_ucs4%white &
                          & // trim(solid_block) // trim(colours_ucs4%reset)
                  else if (n_print.eq.1) then
                      write(*, '(a)', advance='no') colours_ucs4%cyan &
                          & // trim(solid_block) // trim(colours_ucs4%reset)
                  else
                      write(*, '(a)', advance='no') colours_ucs4%blue &
                          & // trim(solid_block) // trim(colours_ucs4%reset)
                  end if

              else
                  ! ---------------Display divergence using x256 colours--------------
                  ! dodgily scale the divergence logarithmically, should end up with nColours+1
                  n_print = int(real(n,DP)/real(n_max,DP) * (exp(real(cmap%nColours+2,DP))+1.0_DP))
                  n_print = n_print_max - (int(log(real(n_print,DP))) - 1)

                  ! n_print = cmap%nColours+1 - int(real(n,DP)/real(n_max,DP))*(cmap%nColours+1)
                  ! if (n_print.gt.0) then
                  !     n_print = int(log(real(n_print,DP)))
                  ! end if

                  if (n.eq.n_max) then
                      write(*, '(a)', advance='no') colours_ucs4%black &
                          & // trim(solid_block) // trim(colours_ucs4%reset)
                  else
                      write(*, '(a)', advance='no') cmap%colourmap(n_print) &
                          & // trim(solid_block) // trim(colours_ucs4%reset)
                  end if

                  ! write(*, '(i1)', advance='no') n_print

              end if

          else
              if (n.eq.n_max) then
                  write(*, '(a)', advance='no') trim(colour) &
                      & // trim(solid_block) // trim(reset)
              else
                  write(*,'(a)', advance='no') ' '
              end if
          end if

       end do
       write(*,*)
    end do

    ! write(*,*) cmap%colourmap(6) // ucs4_'hello there' // colours_ucs4%reset


contains

    subroutine mandel(z, c)
        complex(DP), intent(inout) :: z
        complex(DP), intent(in) :: c
        integer :: z_pow
        z_pow = 2
        z = z**z_pow + c
    end subroutine mandel

end program mandelbrot
