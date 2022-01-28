! modq_test

module modq_demo
  use modq_test
  implicit none

  contains

    subroutine modqtest_example
      type(TestInstance) :: test

      test = TestInstance("My Test Example")

!      call test%failed("My test failed.")

      block ! compare an array
        real :: input(5), output(5)

        input = (/ 1.0, 4.0, 3.0, 4.0, 5.0 /)
        output = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)

        call test%compare_arrays(input, output, "Compare input and output")
      end block
    end subroutine modqtest_example


    subroutine simple_ufbint_example

      block ! get simple variable
        integer, parameter :: lunit = 12

        integer :: ireadmg, ireadsb
        character(8) :: subset
        integer(kind=8) :: my_idate
        integer :: lun, il, im
        integer :: iret
        real(kind=8) :: lat, lon

        open(lunit, file="./testinput/gdas.t18z.1bmhs.tm00.bufr_d")
        call openbf(lunit, "IN", lunit)

        do while (ireadmg(lunit, lun, subset, my_idate) == 0)
          do while (ireadsb(lunit) == 0)
            call ufbint(lunit, lat, 1, 1, iret, "CLAT")
            call ufbint(lunit, lon, 1, 1, iret, "CLON")

            print *, lat, lon
          end do
        end do

        call closbf(lunit)
        close(lunit)
      end block
    end subroutine simple_ufbint_example


    ! Really simple case for getting fixed repeat
    subroutine simple_ufbrep_example

      block ! get simple variable
        integer, parameter :: lunit = 12

        integer :: ireadmg, ireadsb
        character(8) :: subset
        integer(kind=8) :: my_idate
        integer :: lun, il, im
        integer :: iret
        real(kind=8) :: lat, lon
        real(kind=8) :: radiance(5)

        open(lunit, file="/home/rmclaren/Work/ioda-bundle/iodaconv/test/testinput/gdas.t18z.1bmhs.tm00.bufr_d")
        call openbf(lunit, "IN", lunit)

        do while (ireadmg(lunit, lun, subset, my_idate) == 0)
          do while (ireadsb(lunit) == 0)
            call ufbint(lunit, lat, 1, 1, iret, "CLAT")
            call ufbint(lunit, lon, 1, 1, iret, "CLON")
            call ufbrep(lunit, radiance, 1, 5, iret, "TMBR")

            print *, "lat: ", lat, " lon: ", lon, " rad: ",  radiance
          end do
        end do

        call closbf(lunit)
        close(lunit)
      end block
    end subroutine simple_ufbrep_example


end module modq_demo



program demo
  use modq_demo
  implicit none

!  call modqtest_example()
  call simple_ufbint_example
!  call simple_ufbrep_example

end program demo
