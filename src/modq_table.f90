module modq_table
  use moda_tables
  use moda_msgcwd
  use moda_usrint

  use modq_string
  use modq_list
  implicit none

  character(len=3), parameter :: Subset = 'SUB'
  character(len=3), parameter :: DelayedRepetition = 'DRP'
  character(len=3), parameter :: FixedRepetition = 'REP'
  character(len=3), parameter :: DelayedRepStacked = 'DRS'
  character(len=3), parameter :: DelayedBinary = 'DRB'
  character(len=3), parameter :: Sequence = 'SEQ'
  character(len=3), parameter :: StackedRepeat = 'RPS'
  character(len=3), parameter :: Repeat = 'RPC'
  character(len=3), parameter :: Number = 'NUM'
  character(len=3), parameter :: Char = 'CHR'

  private
  public :: all_subsets
  public :: all_queries

contains

  function all_subsets(file_unit) result(subsets)
    integer, intent(in) :: file_unit

    character(8) :: current_subset
    integer :: idx
    integer :: ireadmg
    integer(kind=8) :: my_idate
    logical :: subset_already_found
    type(String), allocatable :: subsets(:)
    type(String), allocatable :: tmp_subsets(:)

    allocate(subsets(0))

    do while (ireadmg(file_unit, current_subset, my_idate) == 0)
      subset_already_found = .false.
      do idx = 1, size(subsets)
        if (subsets(idx) == String(current_subset)) then
          subset_already_found = .true.
          exit
        end if
      end do

      if (.not. subset_already_found) then
        allocate(tmp_subsets(size(subsets) + 1))
        if (size(subsets) > 0 ) tmp_subsets(1:size(subsets)) = subsets(1:size(subsets))
        tmp_subsets(size(tmp_subsets)) = String(current_subset)
        call move_alloc(tmp_subsets, subsets)
      end if
    end do
  end function all_subsets


  function all_queries(file_unit, subset) result(query_strs)
    integer, intent(in) :: file_unit
    type(String), intent(in) :: subset
    type(String), allocatable :: query_strs(:)

    integer :: ireadmg, ireadsb
    character(len=8) :: current_subset
    integer(kind=8) :: my_idate
    integer :: msg_num = 0

    integer :: lun, il, im
    integer :: node_idx, base_idx, path_idx, rewind_idx
    type(String), target, allocatable :: query_bases(:)
    type(String), pointer :: query_bases_ptr(:)
    type(IntList) :: seq_path
    logical :: subset_found
    integer :: query_base_idx

    allocate(query_strs(0))

    call status(file_unit, lun, il, im)

    subset_found = .false.
    do while (ireadmg(file_unit, current_subset, my_idate) == 0)
      msg_num = msg_num + 1

      if (current_subset == subset%chars()) then
        do while ( ireadsb(file_unit) == 0)
          subset_found = .true.
          node_idx = 1
          seq_path = IntList()
          call seq_path%push(inode(lun))  ! Add root node id
          allocate(query_bases(isc(inode(lun)) -  inode(lun)))

          query_base_idx = 1

          do node_idx = inode(lun), isc(inode(lun))
            if (typ(node_idx) == Sequence .or. typ(node_idx) == Repeat .or. typ(node_idx) == StackedRepeat) then
              ! Enter the sequence
              call seq_path%push(node_idx)

            else if (typ(node_idx) == Number .or. typ(node_idx) == Char) then
              ! Found a value
              query_bases(query_base_idx) =  make_query_str(seq_path, node_idx)
              query_base_idx = query_base_idx + 1
            end if

            if (seq_path%length() > 1) then
              ! Peak ahead to see if the next node is inside one of the containing sequences.
              do path_idx = seq_path%length() - 1, 1, -1
                ! Check if the node idx is the next node for the current path
                ! or if the parent node of the next node is the previous path index

                if (seq_path%at(path_idx) == jmpb(node_idx + 1)) then
                  do rewind_idx = 1, seq_path%length() - path_idx
                    ! Exit the sequence
                    call seq_path%pop()
                  end do
                end if
              end do
            end if
          end do

          query_bases_ptr => query_bases

          deallocate(query_strs)
          allocate(query_strs(query_base_idx - 1))
          do base_idx = 1, query_base_idx - 1
            query_strs(base_idx) = add_dup_index(base_idx, query_bases_ptr)
          end do

          exit  ! Capture the table for the first encounter only
        end do
      end if

      if (subset_found) then
        exit
      end if
    end do
  end function all_queries


  type(String) function make_query_str(seq_path, node_idx) result(q_str)
    type(IntList), intent(in) :: seq_path
    integer, intent(in) :: node_idx

    integer :: idx
    character(len=:), allocatable :: q_str_chars

    q_str_chars = trim(tag(seq_path%at(1)))
    do idx = 2, seq_path%length()

      ! Filter out sequences that are not part of the query string
      if (typ(seq_path%at(idx) - 1) == DelayedRepetition .or. &
          typ(seq_path%at(idx) - 1) == FixedRepetition .or. &
          typ(seq_path%at(idx) - 1) == DelayedRepStacked .or. &
          typ(seq_path%at(idx) - 1) == DelayedBinary) then

        q_str_chars = q_str_chars // "/" // trim(tag(seq_path%at(idx)))
      end if
    end do

    q_str_chars = q_str_chars // "/" // tag(node_idx)
    q_str = String(q_str_chars)
  end function make_query_str


  type(String) function add_dup_index(q_idx, queries) result(q_str)
    integer, intent(in) :: q_idx
    type(String), pointer, intent(in) :: queries(:)

    integer :: idx
    integer :: rep_cnt, tot_cnt
    character(len=:), allocatable :: q_str_chars
    character(len=15) :: cnt_str

    tot_cnt = 0
    do idx = 1, size(queries)
      if (queries(q_idx) == queries(idx)) then
        tot_cnt = tot_cnt + 1
      end if
    end do

    q_str_chars = queries(q_idx)%chars()
    if (tot_cnt > 1) then
      rep_cnt = 0
      do idx = 1, q_idx
        if (queries(idx) == queries(q_idx)) then
          rep_cnt = rep_cnt + 1
        end if
      end do

      write (cnt_str, '(i10)') rep_cnt
      q_str_chars = trim(q_str_chars) // "[" // trim(adjustl(cnt_str)) // "]"
    end if

    q_str = String(q_str_chars)
  end function add_dup_index
end module modq_table
