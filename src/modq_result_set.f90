module modq_result_set
  use modq_string
  use modq_list
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

    ! The sub paths that represent a dimension.
    character(len=:), allocatable :: dim_paths(:)
    ! Keeps track of the idxs of dimensions in seq_counts (not all levels in seq_counts represent a new dimension).
    ! These line up with the dimension paths.
    integer, allocatable :: export_dim_idxs(:)

  contains
    final :: data_field__delete
  end type

  interface DataField
    module procedure :: initialize__data_field
  end interface DataField

  type, public :: DataFrame
    type(DataField), allocatable :: data_fields(:)
    integer :: field_count

  contains
    procedure :: add => data_frame__add
    procedure :: field_for_node_named => data_frame__field_for_node_named
    procedure :: field_idx_for_node_named => data_frame__field_idx_for_node_named
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
    data_field = DataField(String(""), String(""), .false., .false., null(), null(), null(), null())
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

  type(DataFrame) function initialize__data_frame(field_count) result(data_frame)
    integer, intent(in) :: field_count

    data_frame = DataFrame(null(), 0)  ! Needed because of gfortran bug
    allocate(data_frame%data_fields(field_count))
  end function initialize__data_frame


  subroutine data_frame__add(self, data_field)
    class(DataFrame), intent(inout) :: self
    type(DataField), intent(in) :: data_field

    self%field_count = self%field_count + 1
    self%data_fields(self%field_count) = data_field
  end subroutine data_frame__add


  function data_frame__field_for_node_named(self, name) result(field)
    class(DataFrame), intent(in) :: self
    type(String), intent(in) :: name

    integer :: field_idx
    logical :: field_found
    type(DataField) :: field

    integer :: cnt = 0

    field_found = .false.
    do field_idx = 1, self%field_count
      if (self%data_fields(field_idx)%name == name) then
        field_found = .true.
        field = self%data_fields(field_idx)
        cnt = cnt + 1
        exit
      end if
    end do

    if (.not. field_found) then
      call bort("Using unknown field named " // name%chars())
    end if
  end function data_frame__field_for_node_named


  function data_frame__field_idx_for_node_named(self, name) result(field_idx)
    class(DataFrame), intent(in) :: self
    type(String), intent(in) :: name

    integer :: field_idx
    logical :: field_found

    field_found = .false.
    do field_idx = 1, self%field_count
      if (self%data_fields(field_idx)%name == name) then
        field_found = .true.
        exit
      end if
    end do

    if (.not. field_found) then
      call bort("Using unknown field named " // name%chars())
    end if
  end function data_frame__field_idx_for_node_named


  subroutine data_frame__delete(self)
    type(DataFrame), intent(inout) :: self

    if (allocated(self%data_fields)) then
      deallocate(self%data_fields)
    end if
  end subroutine data_frame__delete

  ! Result Set Procedures

  type(ResultSet) function initialize__result_set() result(result_set)
    result_set = ResultSet(null(), null(), 0, null())   ! Needed because of gfortran bug

    allocate(result_set%data_frames(DataFrameResizeSize))
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

    real(kind=8), allocatable :: data(:)
    integer :: data_shape(1)

    block ! Check data type of field
      type(DataField) :: target_field

      target_field = self%data_frames(1)%field_for_node_named(String(field_name))
      if (.not. target_field%is_string) then
        call bort(field_name // " is a number field. Use get_as_number to get its value")
      end if
    end block ! Check data type of field

    data = self%get_1d(field_name, group_by)
    data_shape = shape(data)

    allocate(character(8) :: char_data(data_shape(1)))
    char_data = ""

    ! Manually copy each element in the data array
    block ! Move data into char_data)
      integer :: data_row_idx, data_col_idx, char_idx
      integer :: char_cursor_pos
      integer(kind=8) :: data_int_rep

      do data_row_idx = 1, data_shape(1)
        char_cursor_pos = 1
        do char_idx = 0, 7
          data_int_rep = transfer(data(data_row_idx), data_int_rep)
          char_data(data_row_idx)(char_cursor_pos:char_cursor_pos) = &
                  transfer(ibits(data_int_rep, char_idx*8, 8), "a")
          char_cursor_pos = char_cursor_pos + 1
        end do
      end do

    end block ! Move data into char_data
  end function result_set__get_chars


  subroutine result_set__get_rows_for_field(self, target_field, data_rows, dims, groupby_idx)
    class(ResultSet), intent(in) :: self
    type(DataField), intent(in) :: target_field
    real(kind=8), allocatable, intent(inout) :: data_rows(:, :)
    integer, allocatable, intent(in) :: dims(:)
    integer, intent(in), optional :: groupby_idx

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
      inserts = 0

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
      integer :: num_rows
      integer, allocatable :: row_dims(:)
      integer :: nums_per_row

      if (present(groupby_idx)) then
        if (groupby_idx > size(target_field%seq_counts)) then
          num_rows = size(output)

          if (.not. allocated(data_rows)) allocate(data_rows(num_rows, 1))
          data_rows(:, 1) = output(1)
        else
          num_rows = product(dims(1:groupby_idx))
          row_dims = dims(groupby_idx + 1 : size(dims))
          if (.not. allocated(data_rows)) allocate(data_rows(num_rows, product(row_dims)))
          data_rows(:, :) = MissingValue

          nums_per_row = product(row_dims)
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



  subroutine result_set__get_raw_values(self, field_name, data, dims, group_by, dim_paths)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name
    real(kind=8), allocatable, intent(out) :: data(:)
    integer, allocatable, intent(out) :: dims(:)
    character(len=*), intent(in), optional :: group_by
    character(len=:), allocatable, intent(out), optional :: dim_paths(:)

    type(DataField) :: target_field, group_by_field
    integer :: total_rows
    integer :: groupby_idx
    integer, allocatable :: all_dims(:)
    integer :: target_field_idx, group_by_field_idx
    integer, allocatable :: export_dims(:)
    integer :: total_groupby_elements

    total_rows = 0
    total_groupby_elements = 0

    ! Find the dims based on the largest sequence counts in the fields
    block  ! Compute dims
      integer :: frame_idx
      integer :: cnt_idx, export_idx
      integer :: dims_len
      type(IntList) :: dims_list
      integer, allocatable :: tmp_export_dims(:)
      integer :: tmp_tot_groupby_elements


      dims_list = IntList()
      groupby_idx = 1

      ! Get the indices for the fields we care about ahead of time so that we don't
      ! waste time looking for them later. This works because the targets are always
      ! collected in the same order for each frame.
      if (self%data_frames_size > 0) then
        target_field_idx = self%data_frames(1)%field_idx_for_node_named(String(field_name))

        if (present(group_by) .and. group_by /= "") then
          group_by_field_idx = self%data_frames(1)%field_idx_for_node_named(String(group_by))
        end if

        target_field = self%data_frames(1)%data_fields(target_field_idx)
        if (present(dim_paths)) then
          allocate(dim_paths, source=target_field%dim_paths)
        end if

        export_dims = target_field%export_dim_idxs
      end if

      !  Go through the all the frames and find the max of all dimensions and the repetition index of the
      !  groupby field.
      do frame_idx = 1, self%data_frames_size
        target_field = self%data_frames(frame_idx)%data_fields(target_field_idx)

        if (present(dim_paths)) then
          if (size(dim_paths) < size(target_field%dim_paths)) then
            if (allocated(dim_paths)) deallocate(dim_paths)
            allocate(dim_paths, source=target_field%dim_paths)
          end if
        end if

        dims_len = size(target_field%seq_counts)
        if (dims_list%length() < dims_len) then
          call dims_list%resize(dims_len)
        end if

        do cnt_idx = 1, dims_len
          dims_list%at(cnt_idx) = max(dims_list%at(cnt_idx), &
                                      maxval(target_field%seq_counts(cnt_idx)%counts))
        end do

        if (present(group_by) .and. group_by /= "") then
          group_by_field = self%data_frames(frame_idx)%data_fields(group_by_field_idx)
          groupby_idx = max(groupby_idx, size(group_by_field%seq_counts))

          if (present(dim_paths)) then
            if (groupby_idx > dims_list%length()) then
              dim_paths = group_by_field%dim_paths(size(group_by_field%dim_paths):size(group_by_field%dim_paths))

              tmp_tot_groupby_elements = 1
              do cnt_idx = 1, size(group_by_field%seq_counts)
                tmp_tot_groupby_elements = tmp_tot_groupby_elements * maxval(group_by_field%seq_counts(cnt_idx)%counts)
              end do

              if (tmp_tot_groupby_elements > total_groupby_elements) then
                total_groupby_elements = tmp_tot_groupby_elements
              end if
            else
              dim_paths = target_field%dim_paths(size(group_by_field%export_dim_idxs):size(target_field%dim_paths))
            end if
          end if
        end if
      end do

      all_dims = dims_list%array()
      if (present(group_by) .and. group_by /= "") then
        ! The groupby field occurs at the same or greater repetition level as the target field.
        if (groupby_idx > dims_list%length()) then
          allocate(dims(1))
          dims(1) = total_groupby_elements
          export_dims = [1]
          all_dims = dims

        ! The groupby field occurs at a lower repetition level than the target field.
        else
          allocate(dims(dims_list%length() - groupby_idx + 1))
          dims(1) = product(all_dims(1:groupby_idx))
          dims(2:size(dims)) = all_dims(groupby_idx+1:size(all_dims))
          export_dims = export_dims - groupby_idx + 1

          tmp_export_dims = [1]
          do export_idx = 1, size(export_dims)
            if (export_dims(export_idx) > 0) then
              tmp_export_dims = [tmp_export_dims, export_dims(export_idx)]
            end if
          end do
          export_dims = tmp_export_dims
        end if
      ! There is no groupby field. So use all the dimensions.
      else
        allocate(dims, source=all_dims)
      end if

      total_rows = dims(1) * self%data_frames_size
    end block  ! Compute dims
    

    block  ! Make data set
      real(kind=8), allocatable :: frame_data(:,:)
      integer :: frame_idx
      integer :: data_row_idx
      integer :: row_idx
      integer :: row_length

      row_length = max(product(dims(2:size(dims))), 1)

      allocate(data(total_rows*row_length))
      data = MissingValue

      do frame_idx = 1, self%data_frames_size
        target_field = self%data_frames(frame_idx)%data_fields(target_field_idx)

        if (.not. target_field%missing) then
          if (present(group_by) .and. group_by /= "") then

            call self%get_rows_for_field(target_field, & 
                                          frame_data, &
                                          all_dims, &
                                          groupby_idx)
          else
            call self%get_rows_for_field(target_field, &
                                          frame_data, &
                                          all_dims)
          end if

          data_row_idx = dims(1) * (frame_idx - 1)

          do row_idx = 1, dims(1)
            data(data_row_idx*row_length + 1:data_row_idx*row_length + row_length) = &
              frame_data(row_idx, :)
            data_row_idx = data_row_idx + 1
          end do
        end if
      end do

      ! Convert dims per data frame to dims for all the collected data.
      dims(1) = total_rows
      ! Throw away dims we don't want. These will be dims associated with binary reps
      ! ex: "<ABC>" who are repeated at most once.
      if (self%data_frames_size > 1) then
        dims = dims(export_dims)
      end if

    end block  ! Make data set
  end subroutine


  ! Check if the field is a string
  function result_set__is_string(self, field_name) result(is_string)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name

    type(DataField) :: target_field
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

    character(len=20) :: size_dims_str

    write (size_dims_str, *) size(raw_dims)

    if (size(raw_dims) /= size(dims)) then
      call bort("Error: data has the wrong number of dims. Try get_" // &
                trim(adjustl(size_dims_str)) // "d instead.")
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

