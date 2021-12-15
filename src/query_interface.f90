module query_interface
  use bufr_c_interface_mod, only:c_f_string
  use modq_query_set
  use modq_execute
  use modq_result_set

  use iso_c_binding
  implicit none

  real(kind=c_double), allocatable, target, save :: data_f_ptr(:)
  integer(kind=c_int), allocatable, target, save :: dims_ptr(:)
  character(len=:), allocatable, target, save :: dim_paths_f(:)

  contains

! Query Set Methods
  subroutine query_set__allocate(query_set_cptr) bind(C, name='query_set__allocate_f')
    type(c_ptr), intent(inout) :: query_set_cptr
    type(QuerySet), pointer :: query_set_fptr

    allocate(query_set_fptr)
    query_set_cptr = c_loc(query_set_fptr)
  end subroutine query_set__allocate

  subroutine query_set__add_c(cls, query_str, query_name) bind(C, name='query_set__add_f')
    type(c_ptr), intent(in) :: cls
    character(kind=c_char, len=1), intent(in) :: query_str
    character(kind=c_char, len=1), intent(in) :: query_name

    type(QuerySet), pointer :: query_set
    call c_f_pointer(cls, query_set)

    call query_set%add(c_f_string(query_str), c_f_string(query_name))
  end subroutine query_set__add_c

  subroutine query_set__print_c(cls) bind(C, name='query_set__print_f')
    type(c_ptr), intent(in) :: cls

    type(QuerySet), pointer :: query_set
    call c_f_pointer(cls, query_set)

    call query_set%print()
  end subroutine query_set__print_c

  subroutine query_set__deallocate(query_set_cptr) bind(C, name='query_set__deallocate_f')
    type(c_ptr), value :: query_set_cptr
    type(QuerySet), pointer :: query_set_fptr

    call c_f_pointer(query_set_cptr, query_set_fptr)
    deallocate(query_set_fptr)
  end subroutine query_set__deallocate


  ! Execute Functions
  subroutine execute_c(file_unit, query_set_c, next, result_set_c) bind(C, name='execute_f')
    integer(c_int), value, intent(in) :: file_unit
    type(c_ptr), intent(in) :: query_set_c
    integer(c_int), value, intent(in) :: next
    type(c_ptr), intent(inout) :: result_set_c

    type(QuerySet), pointer :: query_set_f
    type(ResultSet), pointer :: result_set_f
    integer :: int_type

    call c_f_pointer(query_set_c, query_set_f)
    call c_f_pointer(result_set_c, result_set_f)

    call execute_(transfer(file_unit, int_type), &
                  query_set_f, &
                  result_set_f, &
                  transfer(next, int_type))
  end subroutine execute_c

  ! Result Set Methods
  subroutine result_set__allocate(result_set_cptr) bind(C, name='result_set__allocate_f')
    type(c_ptr), intent(inout) :: result_set_cptr
    type(ResultSet), pointer :: result_set_fptr

    allocate(result_set_fptr)
    allocate(result_set_fptr%data_frames(DataFrameResizeSize))

    result_set_cptr = c_loc(result_set_fptr)
  end subroutine result_set__allocate


  subroutine result_set__get_raw_c(cls, field, group_by_field, data, dims, num_dims, dim_paths, dim_paths_str_len) &
    bind(C, name="result_set__get_raw_f")

    type(c_ptr), intent(inout) :: cls
    character(kind=c_char, len=1), intent(in) :: field
    character(kind=c_char, len=1), intent(in) :: group_by_field
    type(c_ptr), intent(inout) :: data
    type(c_ptr), intent(inout) :: dims
    integer(kind=c_int), intent(out) :: num_dims
    type(c_ptr), intent(out) :: dim_paths
    integer(kind=c_int), intent(out) :: dim_paths_str_len

    character(len=:), allocatable :: f_field, f_group_by_field
    real(kind=8), allocatable :: data_f(:)
    integer, allocatable :: dims_f(:)

    integer(kind=c_int), allocatable :: dims_c(:)

    type(ResultSet), pointer :: result_set_fptr
    call c_f_pointer(cls, result_set_fptr)

    f_field = c_f_string(field)
    f_group_by_field = c_f_string(group_by_field)

    call result_set_fptr%get_raw_values(f_field, data_f, dims_f, f_group_by_field, dim_paths_f)

    num_dims = size(dims_f, kind=c_int)
    allocate(dims_c(size(dims_f)))
    dims_c(1:size(dims_c)) = transfer(dims_f(1:size(dims_c)), dims_c(1:size(dims_c)))

    if (product(dims_f) > 0) then
      allocate(data_f_ptr, source=data_f)
      allocate(dims_ptr, source=dims_c)
      data = c_loc(data_f_ptr(1))
      dims = c_loc(dims_ptr(1))
      dim_paths = c_loc(dim_paths_f)
      dim_paths_str_len = len(dim_paths_f(1))
    end if
  end subroutine result_set__get_raw_c


  function result_set__is_string_c(cls, field) result(is_string) &
    bind(C, name="result_set__is_string_f")
    
    type(c_ptr), intent(inout) :: cls
    character(kind=c_char, len=1), intent(in) :: field
    logical(kind=c_bool) :: is_string

    type(ResultSet), pointer :: result_set_fptr

    call c_f_pointer(cls, result_set_fptr)
    is_string = result_set_fptr%is_string(c_f_string(field))
  end function result_set__is_string_c
  

  subroutine result_set__deallocate(result_set_cptr) bind(C, name='result_set__deallocate_f')
    type(c_ptr), value :: result_set_cptr
    type(ResultSet), pointer :: result_set_fptr

!    result_set_fptr => null()

    call c_f_pointer(result_set_cptr, result_set_fptr)
    deallocate(result_set_fptr)
  end subroutine result_set__deallocate


  subroutine free_result_get_data_c() bind(C, name='free_result_get_data_f')
    if (allocated(data_f_ptr)) deallocate(data_f_ptr)
    if (allocated(dims_ptr)) deallocate(dims_ptr)
    if (allocated(dim_paths_f)) deallocate(dim_paths_f)
  end subroutine free_result_get_data_c

end module query_interface
