module modq_result_set
  use modq_string
  implicit none

  private

  real(kind=8), public, parameter :: MissingValue = 10e10_8
  integer, parameter :: DataFrameResizeSize = 50000

  type, public :: SeqCounts
    integer, allocatable :: counts(:)
  end type

  interface SeqCounts
    module procedure :: initialize__seq_counts
  end interface SeqCounts

  type, public :: DataField
    type(String) :: name
    type(String) :: query_str
    logical :: is_string
    logical :: missing = .false.
    real(kind=8), allocatable :: data(:)
    integer, allocatable :: seq_path(:)
    type(SeqCounts), allocatable :: seq_counts(:)

  contains
    final :: data_field__delete
  end type

  interface DataField
    module procedure :: initialize__data_field
  end interface DataField

  type, public :: DataFrame
    type(DataField), allocatable :: data_fields(:)

  contains
    procedure :: add => data_frame__add
    procedure :: field_for_node_named => data_frame__field_for_node_named
    final ::  data_frame__delete
  end type

  interface DataFrame
    module procedure :: initialize__data_frame
  end interface DataFrame

  type, public :: ResultSet

    type(DataFrame), allocatable :: data_frames(:)
    type(String), allocatable :: names(:)
    integer :: data_frames_size = 0
    integer, allocatable :: field_widths(:)

    contains
      private
      procedure, public :: get_1d => result_set__get_1d
      procedure, public :: get_2d => result_set__get_2d
      procedure, public :: get_3d => result_set__get_3d
      procedure, public :: get_4d => result_set__get_4d
      procedure, public :: get_chars => result_set__get_chars   
      procedure, public :: get_raw_values => result_set__get_raw_values
      procedure, public :: is_string => result_set__is_string
      procedure, public :: add => result_set__add
      procedure :: get_rows_for_field => result_set__get_rows_for_field
      procedure :: check_dims => result_set__check_dims
      final :: result_set__delete
  end type ResultSet

  interface ResultSet
    module procedure :: initialize__result_set
  end interface ResultSet

contains
  ! Sequence Counts
  type(SeqCounts) function initialize__seq_counts() result(seq_counts)
    seq_counts = SeqCounts(null())
    allocate(seq_counts%counts(0))
  end function

  ! Data Field Procedures
  type(DataField) function initialize__data_field() result(data_field)
    ! Needed because of gfortran bug
    data_field = DataField(String(""), String(""), .false., .false., null(), null(), null())

    allocate(data_field%data(0))
    allocate(data_field%seq_path(0))
  end function initialize__data_field

  subroutine data_field__delete(self)
    type(DataField), intent(inout) :: self

    if (allocated(self%data)) then
      deallocate(self%data)
    end if

    if (allocated(self%seq_path)) then
      deallocate(self%seq_path)
    end if

    if (allocated(self%seq_counts)) then
      deallocate(self%seq_counts)
    end if

  end subroutine data_field__delete

  ! Data Frame Procedures

  type(DataFrame) function initialize__data_frame() result(data_frame)
    data_frame = DataFrame(null())  ! Needed because of gfortran bug
  end function initialize__data_frame
!
  subroutine data_frame__add(self, data_field)
    class(DataFrame), intent(inout) :: self
    type(DataField), intent(in) :: data_field

    type(DataField), allocatable :: tmp_data_field(:)

    if (.not. allocated(self%data_fields)) then
      allocate(self%data_fields(0))
    end if

    allocate(tmp_data_field(size(self%data_fields) + 1))
    tmp_data_field(1:size(self%data_fields)) = self%data_fields
    tmp_data_field(size(tmp_data_field)) = data_field
    call move_alloc(tmp_data_field, self%data_fields) 

  end subroutine data_frame__add


  function data_frame__field_for_node_named(self, name) result(field)
    class(DataFrame), intent(in) :: self
    type(String), intent(in) :: name

    integer :: field_idx
    logical :: field_found
    type(DataField), allocatable :: field

    field_found = .false.
    do field_idx = 1, size(self%data_fields)
      if (self%data_fields(field_idx)%name == name) then
        field = self%data_fields(field_idx)
        field_found = .true.
        exit
      end if
    end do

    if (.not. field_found) then
      call bort("Using unknown field named " // name%chars())
    end if
  end function data_frame__field_for_node_named


  subroutine data_frame__delete(self)
    type(DataFrame), intent(inout) :: self

    if (allocated(self%data_fields)) then
      deallocate(self%data_fields)
    end if
  end subroutine data_frame__delete

  ! Result Set Procedures

  type(ResultSet) function initialize__result_set() result(result_set)
    result_set = ResultSet(null(), null(), 0, null())   ! Needed because of gfortran bug

    allocate(result_set%data_frames(0))
    allocate(result_set%names(0))
    allocate(result_set%field_widths(0))
  end function initialize__result_set


  function result_set__get_1d(self, target_name, group_by) result(data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: target_name
    character(len=*), intent(in), optional :: group_by
    real(kind=8), allocatable :: data(:)
  
    integer :: dims(1)
    real(kind=8), allocatable :: raw_data(:)
    integer, allocatable :: raw_dims(:)
  
    call self%get_raw_values(target_name, raw_data, raw_dims, group_by)
    call self%check_dims(raw_dims, dims)
    dims = raw_dims
  
    allocate(data(dims(1)))
    data = reshape(raw_data, dims)
  end function
  
  
  function result_set__get_2d(self, target_name, group_by) result(data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: target_name
    character(len=*), intent(in), optional :: group_by
    real(kind=8), allocatable :: data(:, :)
  
    integer :: dims(2)
    real(kind=8), allocatable :: raw_data(:)
    integer, allocatable :: raw_dims(:)
  
    call self%get_raw_values(target_name, raw_data, raw_dims, group_by)
    call self%check_dims(raw_dims, dims)
    dims = raw_dims
  
    allocate(data(dims(1), dims(2)))
    data = reshape(raw_data, dims)
  end function
  
  
  function result_set__get_3d(self, target_name, group_by) result(data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: target_name
    character(len=*), intent(in), optional :: group_by
    real(kind=8), allocatable :: data(:, :, :)
  
    integer :: dims(3)
    real(kind=8), allocatable :: raw_data(:)
    integer, allocatable :: raw_dims(:)
  
    call self%get_raw_values(target_name, raw_data, raw_dims, group_by)
    call self%check_dims(raw_dims, dims)
    dims = raw_dims
  
    allocate(data(dims(1), dims(2), dims(3)))
    data = reshape(raw_data, dims)
  end function
  
  
  function result_set__get_4d(self, target_name, group_by) result(data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: target_name
    character(len=*), intent(in), optional :: group_by
    real(kind=8), allocatable :: data(:,:,:,:)
  
    integer :: dims(4)
    real(kind=8), allocatable :: raw_data(:)
    integer, allocatable :: raw_dims(:)
  
    call self%get_raw_values(target_name, raw_data, raw_dims, group_by)
    call self%check_dims(raw_dims, dims)
    dims = raw_dims
  
    allocate(data(dims(1), dims(2), dims(3), dims(4)))
    data = reshape(raw_data, dims)
  end function


  function result_set__get_chars(self, field_name, group_by) result(char_data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: group_by
    character(:), allocatable :: char_data(:)

    real(kind=8), allocatable :: data(:, :)
    integer :: data_shape(2)

    block ! Check data type of field
      type(DataField), allocatable :: target_field

      target_field = self%data_frames(1)%field_for_node_named(String(field_name))
      if (.not. target_field%is_string) then
        call bort(field_name // " is a number field. Use get_as_number to get its value")
      end if
    end block ! Check data type of field

    data = self%get_2d(field_name, group_by)
    data_shape = shape(data)

    allocate(character(data_shape(2) * 8) :: char_data(data_shape(1)))
    char_data = ""

    ! Manually copy each element in the data array
    block ! Move data into char_data)
      integer :: data_row_idx, data_col_idx, char_idx
      integer :: char_cursor_pos
      integer(kind=8) :: data_int_rep

      do data_row_idx = 1, data_shape(1)
        char_cursor_pos = 1
        do data_col_idx = 1, data_shape(2)
          do char_idx = 0, 7
            data_int_rep = transfer(data(data_row_idx, data_col_idx), data_int_rep)
            char_data(data_row_idx)(char_cursor_pos:char_cursor_pos) = &
                    transfer(ibits(data_int_rep, char_idx*8, 8), "a")
            char_cursor_pos = char_cursor_pos + 1
          end do
        end do
      end do

    end block ! Move data into char_data
  end function result_set__get_chars


  subroutine result_set__get_rows_for_field(self, target_field, data_rows, dims, group_by_field)
    class(ResultSet), intent(in) :: self
    type(DataField), intent(in) :: target_field
    real(kind=8), allocatable, intent(inout) :: data_rows(:, :)
    integer, allocatable, intent(in) :: dims(:)
    type(DataField), intent(in), optional :: group_by_field

    integer :: idx
    integer, allocatable :: inserts(:, :)
    integer :: max_counts
    integer, allocatable :: idxs(:)
    real(kind=8) :: output(product(dims))

    max_counts = 0
    allocate(idxs(size(target_field%data)))
    idxs = (/(idx, idx=1, size(idxs), 1)/)

    block  ! compute max counts
      integer :: dim_idx

      do dim_idx = 1, size(dims)
        ! Update max_counts by size of counts array
        if (max_counts < size(target_field%seq_counts(dim_idx)%counts)) then
          max_counts = size(target_field%seq_counts(dim_idx)%counts)
        end if
      end do
    end block  ! compute max counts

    block ! Compute insert array
      integer :: rep_idx

      allocate(inserts(size(dims), max_counts))

      do rep_idx = 1, size(target_field%seq_counts)
        ! inserts = total elements for this dim - number accounted for by reps
        inserts(rep_idx, :) = product(dims(rep_idx:)) - &
                              target_field%seq_counts(rep_idx)%counts * product(dims(rep_idx+1:))
      end do
    end block ! Compute insert array

    ! Inflate the data, compute the idxs for each data element in the result array
    block  ! Compute data idxs
      integer :: dim_idx, insert_idx, data_idx
      integer :: num_inserts

      do dim_idx = size(dims), 1, -1
        do insert_idx = 1, size(inserts(dim_idx, :))
          num_inserts = inserts(dim_idx, insert_idx)
          if (num_inserts > 0) then
            data_idx = product(dims(dim_idx:)) * insert_idx + product(dims(dim_idx:)) - num_inserts

            where (idxs >= data_idx)
              idxs = idxs + num_inserts
            end where
          end if
        end do
      end do

    end block  ! Compute data idxs


    ! Inflate output
    output = MissingValue
    output(idxs) = target_field%data


    block  ! Apply group_by and make output
      integer :: row_idx
      integer :: groupby_rep_idx
      integer :: num_rows, nums_per_row

      if (present(group_by_field)) then
        groupby_rep_idx = size(group_by_field%seq_counts)
        if (groupby_rep_idx > size(target_field%seq_counts)) then
          num_rows = size(output)

          if (.not. allocated(data_rows)) allocate(data_rows(num_rows, 1))
          data_rows(:, 1) = output
        else
          num_rows = product(dims(1:groupby_rep_idx))
          if (.not. allocated(data_rows)) allocate(data_rows(num_rows, product(dims)))
          data_rows(:, :) = MissingValue

          nums_per_row = product(dims)
          do row_idx = 0, (num_rows - 1)
            data_rows(row_idx + 1, :) = output(nums_per_row * row_idx + 1 : &
                                               nums_per_row * row_idx + nums_per_row)
          end do
        end if
      else
        if (.not. allocated(data_rows)) allocate(data_rows(1, size(output)))
        data_rows(1, :) = output
      end if
    end block  ! Apply group_by and make output
  end subroutine


  subroutine result_set__get_raw_values(self, field_name, data, dims, group_by)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name
    real(kind=8), allocatable, intent(out) :: data(:)
    integer, allocatable, intent(out) :: dims(:)
    character(len=*), intent(in), optional :: group_by

    type(DataField) :: target_field, group_by_field
    integer :: total_rows

    total_rows = 0

    ! Find the dims based on the largest sequence counts in the fields
    block  ! Compute dims
      integer :: frame_idx
      integer :: cnt_idx

      do frame_idx = 1, self%data_frames_size
        target_field = self%data_frames(frame_idx)%field_for_node_named(String(field_name))
        if (present(group_by).and. group_by /= "") then
          group_by_field = self%data_frames(frame_idx)%field_for_node_named(String(group_by))

          if (size(group_by_field%seq_counts) < size(target_field%seq_counts)) then
            if (.not. allocated(dims)) then
              allocate(dims(size(target_field%seq_counts) - size(group_by_field%seq_counts) + 1))
              dims = 0
            end if

            do cnt_idx = size(group_by_field%seq_counts), size(target_field%seq_counts)
              dims(cnt_idx) = max(dims(cnt_idx), maxval(target_field%seq_counts(cnt_idx)%counts))
            end do
          else
            if (.not. allocated(dims)) then
              allocate(dims(1))
              dims = 1
              exit
            end if
          end if
        else
          if (.not. allocated(dims)) then
            allocate(dims(size(target_field%seq_counts)))
            dims = 0
          end if

          do cnt_idx = 1, size(dims)
            dims(cnt_idx) = max(dims(cnt_idx), maxval(target_field%seq_counts(cnt_idx)%counts))
          end do
        end if

        total_rows = total_rows + dims(1)
      end do
    end block  ! Compute dims

    block  ! Make data set
      real(kind=8), allocatable :: frame_data(:,:)
      integer :: frame_idx
      integer :: data_row_idx
      integer :: row_idx

      allocate(data(total_rows*product(dims)))
      data = MissingValue

      do frame_idx = 1, self%data_frames_size
        target_field = self%data_frames(frame_idx)%field_for_node_named(String(field_name))

        if (target_field%missing) then
          ! Add a missing as missing value
          data_row_idx = dims(1) * (frame_idx - 1)
          data(data_row_idx*product(dims) + 1:data_row_idx*product(dims) + product(dims)) &
            = MissingValue
        else
          if (present(group_by) .and. group_by /= "") then
            group_by_field = self%data_frames(frame_idx)%field_for_node_named(String(group_by))

            call self%get_rows_for_field(target_field, & 
                                          frame_data, &
                                          dims, &
                                          group_by_field)
          else
            call self%get_rows_for_field(target_field, &
                                          frame_data, &
                                          dims)
          end if
        end if

        data_row_idx = dims(1) * (frame_idx - 1)
        do row_idx = 1, dims(1)
          data(data_row_idx*product(dims) + 1:data_row_idx*product(dims) + product(dims)) = &
            frame_data(row_idx, :)
          data_row_idx = data_row_idx + 1
        end do
      end do

      dims(1) = total_rows
    end block  ! Make data set

  end subroutine


  ! Check if the field is a string
  function result_set__is_string(self, field_name) result(is_string)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name

    type(DataField), allocatable :: target_field
    logical :: is_string
    is_string = .false.
      
    target_field = self%data_frames(1)%field_for_node_named(String(field_name))
    is_string = target_field%is_string
  end function result_set__is_string


  subroutine result_set__add(self, data_frame)
    class(ResultSet), intent(inout) :: self
    type(DataFrame), intent(in) :: data_frame

    integer :: field_idx, name_idx
    logical :: name_found
    type(DataField) :: field
    type(DataFrame), allocatable :: tmp_data_frames(:)
    type(String), allocatable :: tmp_names(:)

    if (.not. allocated(self%data_frames)) then
      allocate(self%data_frames(0))
    end if

    do field_idx = 1, size(data_frame%data_fields)
      field = data_frame%data_fields(field_idx)

      name_found = .false.
      do name_idx = 1, size(self%names)
        if (field%name == self%names(name_idx)) then
          self%field_widths(name_idx) = max(size(field%data), self%field_widths(name_idx))
          name_found = .true.
          exit
        end if
      end do

      if (.not. name_found) then
        allocate(tmp_names(size(self%names) + 1))
        tmp_names(1:size(self%names)) = self%names
        tmp_names(size(tmp_names)) = field%name
        call move_alloc(tmp_names, self%names)

        self%field_widths = [self%field_widths, size(field%data)]
      end if
    end do

    if (self%data_frames_size >= size(self%data_frames)) then
      allocate(tmp_data_frames(self%data_frames_size + DataFrameResizeSize))
      tmp_data_frames(1:self%data_frames_size) = self%data_frames
      call move_alloc(tmp_data_frames, self%data_frames)
    end if

    self%data_frames_size = self%data_frames_size + 1
    self%data_frames(self%data_frames_size) = data_frame

  end subroutine result_set__add


  subroutine result_set__check_dims(self, raw_dims, dims)
    class(ResultSet), intent(in) :: self
    integer, intent(in) :: raw_dims(:)
    integer, intent(in) :: dims(:)

    if (size(raw_dims) /= size(dims)) then
      ! call bort("Error: data has the wrong number of dims. Try get_" // size(raw_dims) // "d instead.")
    end if
  end subroutine result_set__check_dims


  subroutine result_set__delete(self)
    type(ResultSet), intent(inout) :: self

    if (allocated(self%names)) then
      deallocate(self%names)
    end if

    if (allocated(self%data_frames)) then
      deallocate(self%data_frames)
    end if
  end subroutine result_set__delete

end module modq_result_set

