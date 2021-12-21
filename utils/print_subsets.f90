
subroutine print_help
  implicit none

  print *, "Usage:", "./print_subsets.x [-h] <input_file>"
  print *, "Description: "
  print *, "  Lists all the subsets contained in a BUFR file."
  print *, "Arguments: "
  print *, "  -h          (Optional) Print out the help message."
  print *, "  input_file  Path to the BUFR file."
  print *, "Example:", "./print_subsets.x ./bufr_satwnd_old_format.bufr"
end subroutine print_help


program print_subsets
  use modq_string 
  use modq_table
  implicit none

  integer, parameter :: FileUnit = 12
  integer, parameter :: FileUnitTable1 = 13
  integer, parameter :: FileUnitTable2 = 14

  character(len=255) :: input_file
  character(len=255) :: table_path
  character(len=255) :: arg
  type(String), allocatable :: subsets(:)
  integer :: idx

  input_file = ""

  idx = 1
  do while (idx <= iargc())
    call getarg(idx, arg)
    if (arg == "-h") then
      call print_help
      return
    else if (arg == "-t") then
      call getarg(idx+1, table_path)
      idx = idx + 2
    else
      call getarg(idx, input_file)
      idx = idx + 1
    end if
  end do

  if (trim(input_file) == "") then
    call print_help
    call bort("Error: no input file specified")
  end if

  open(FileUnit, file=input_file)

  if (table_path == "") then
    call openbf(FileUnit, "IN", FileUnit)
  else
    call openbf(FileUnit, "SEC3", FileUnit)
    call mtinfo(trim(table_path), FileUnitTable1, FileUnitTable2)
  end if

  subsets = all_subsets(FileUnit)

  print *, "Available subsets:"
  do idx = 1, size(subsets)
    print *, "  ", subsets(idx)%chars()
  end do

  close(FileUnit)
end program print_subsets