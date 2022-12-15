        program atom_test

            implicit none
            integer :: ios
            integer, parameter :: read_unit = 99
            character(len=200), allocatable :: command(:)
            character(len=200) :: line
            integer :: n, i

            open(unit=read_unit, file="documents/SS_BT12_SS30.rst")
*            if ( ios /= 0 ) stop "Error opening file data.dat"

            n = 0

            do
                read(read_unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                n = n + 1
            end do

            print*, "File contains ", n, "commands"

            allocate(command(n))

            rewind(read_unit)

            do i = 1, n
                read(read_unit, '(A)') command(i)
            end do

            close(read_unit)

            do i = 1, n
                print*, command(i)
            end do

        end program atom_test