!> @file
!> @brief Module that contains functions that execute a query
!>
!> @author Ronald Mclaren
!> @date 2021-05-11
!>

module modq_query
  use moda_tables
  use moda_msgcwd
  use moda_usrint

  use modq_list
  use modq_query_set
  use modq_result_set
  use modq_string
  use modq_query_parser

  implicit none


  !> @author Ronald Mclaren
  !> @date 2021-05-11
  !>
  !> @brief Data structure that holds all the information about a specific target
  !>        node derived from a query string.
  !>
  type, private :: Target
    !> @brief The name assinged to the target node
    character(len=:), allocatable :: name
    !> @brief The query string for this target node
    character(len=:), allocatable :: query_str
    !> @brief Does this target point to a string or real field.
    logical :: is_string
    !> @brief The list of sequences leading to this target
    integer, allocatable :: seq_path(:)
    !> @brief The list of node ID's associated with this target
    integer, allocatable :: node_ids(:)
    !> @brief List of query paths which define the datas dimensions
    character(len=:), allocatable :: dim_paths(:)
    !> @brief List of branch idxs of dimensioned elements.
    integer, allocatable :: export_dim_idxs(:)
  end type Target


  type, private :: ProcessingMasks
    logical, allocatable :: value_node_mask(:)
    logical, allocatable :: path_node_mask(:)
  end type ProcessingMasks


  !> @author Ronald Mclaren
  !> @date 2021-11-22
  !>
  !> @brief Data structure that holds all the values for all possible node id's for a message. Basically a reorganized
  !>        version of the VAL array from the legacy BUFR code.
  !>
  type, private :: NodeValueTable
    integer :: start_node_id
    type(RealList), allocatable :: value_lists(:)
    type(IntList), allocatable :: count_lists(:)

    contains
      procedure, public :: values_for_node => node_value_table__values_for_node
      procedure, public :: counts_for_node => node_value_table__counts_for_node
      procedure, public :: delete => node_value_table__delete
  end type NodeValueTable

  interface NodeValueTable
    module procedure :: init__node_value_table
  end interface NodeValueTable


  private
  character(len=3), parameter :: Subset = 'SUB'
  character(len=3), parameter :: DelayedRep = 'DRP'
  character(len=3), parameter :: FixedRep = 'REP'
  character(len=3), parameter :: DelayedRepStacked = 'DRS'
  character(len=3), parameter :: DelayedBinary = 'DRB'
  character(len=3), parameter :: Sequence = 'SEQ'
  character(len=3), parameter :: Repeat = 'RPC'
  character(len=3), parameter :: StackedRepeat = 'RPS'
  public::query

contains


!> @author Ronald Mclaren
!> @date 2021-06-07
!>
!> @brief Executes a query
!>
!> @param[in] lunit - integer: The file unit number
!> @param[in] current_subset - type(String): The current subset string
!> @param[in] query_set - type(QuerySet): The query set
!> @param[inout] result_set - type(ResultSet): The result set to add to
!>
  subroutine query(lunit, current_subset, query_set, result_set)
    integer, intent(in) :: lunit
    type(String), intent(in) :: current_subset
    type(QuerySet), intent(in) :: query_set
    type(ResultSet), intent(inout) :: result_set

    integer :: lun, il, im
    type(Target), allocatable :: targets(:)
    type(ProcessingMasks) :: masks

    call status(lunit, lun, il, im)

    call find_targets(lun, current_subset, query_set, targets, masks)
    call collect_data(lun, targets, masks, result_set)

  end subroutine query


  !> @author Ronald Mclaren
  !> @date 2021-06-08
  !>
  !> @brief Finds the targets for all the queries in a query set 
  !> in the query set.  The function is memoized which means that it will
  !> only run the query once for each subset type (caches the result).
  !>
  !> @param[in] lun - integer: Unit number for the BUFR file
  !> @param[in] current_subset - type(String): The current subset string
  !> @param[in] query_set - type(QuerySet): The query set
  !> @return targets - type(Target)(:): The list of targets
  !>
  subroutine find_targets(lun, current_subset, query_set, targets, masks)
    integer, intent(in) :: lun
    type(String), intent(in) :: current_subset
    type(QuerySet), intent(in) :: query_set
    type(Target), allocatable, intent(out) :: targets(:)
    type(ProcessingMasks), intent(out) :: masks

    ! These variables are used to memoize this function (make it remember previous results to save time)
    type TargetList
      type(Target), allocatable :: targets(:)
    end type TargetList
    character(len=10), allocatable, save :: stored_subsets(:)
    type(TargetList), allocatable, save :: stored_target_list(:)
    type(ProcessingMasks), allocatable, save :: stored_masks(:)

    if (.not. allocated(stored_subsets)) then
      allocate(stored_subsets(0))
      allocate(stored_target_list(0))
      allocate(stored_masks(0))
    end if

    block  ! Find targets in stored target list
      integer :: subset_idx

      do subset_idx = 1, size(stored_subsets)
        if (stored_subsets(subset_idx) == current_subset%chars()) then
          targets = stored_target_list(subset_idx)%targets
          masks = stored_masks(subset_idx)
          exit
        end if
      end do
    end block

    if (.not. allocated(targets)) then
      block  ! Find targets for new subset
        ! These variables are used to
        integer :: q_idx, target_idx, path_idx
        character(len=:), allocatable :: name
        type(String) :: query_str
        type(String), allocatable :: query_strs(:)
        type(Target), allocatable :: tmp_targets(:)
        type(Target) :: targ
        logical :: found_target
        type(TargetList), allocatable :: tmp_stored_target_list(:)
        type(ProcessingMasks), allocatable :: tmp_stored_masks(:)

        allocate(targets(0))
        allocate(masks%value_node_mask(isc(inode(lun))))
        allocate(masks%path_node_mask(isc(inode(lun))))
        masks%value_node_mask = .false.
        masks%path_node_mask = .false.
        do target_idx = 1,query_set%count()
          name = query_set%get_query_name(target_idx)
          query_strs = split_into_subqueries(query_set%get_query_str(target_idx))

          found_target = .false.
          do q_idx = 1, size(query_strs)
            query_str = query_strs(q_idx)
            targ = find_target(lun, current_subset, name, query_str%chars())

            if (size(targ%node_ids) /= 0) then
              ! Collect mask data
              masks%value_node_mask(targ%node_ids(1)) = .true.
              do path_idx = 1, size(targ%seq_path)
                masks%path_node_mask(targ%seq_path(path_idx)) = .true.
              end do

              ! Fortran runtime does not deallocate memory correctly if you do
              ! targets = [targets, find_target(lun, name, query_str)]
              allocate(tmp_targets(size(targets) + 1))
              tmp_targets(1:size(targets)) = targets(1:size(targets))
              tmp_targets(size(tmp_targets)) = targ
              deallocate(targets)
              call move_alloc(tmp_targets, targets)

              found_target = .true.
              exit
            end if
          end do

          if (.not. found_target) then
            ! Add the last missing target to the list
            allocate(tmp_targets(size(targets) + 1))
            tmp_targets(1:size(targets)) = targets(1:size(targets))
            tmp_targets(size(tmp_targets)) = targ
            deallocate(targets)
            call move_alloc(tmp_targets, targets)

            print *, "Warning: Query String "  &
                     // query_set%get_query_str(target_idx) &
                     // " didn't apply to subset " &
                     // current_subset%chars()
          end if
        end do

        allocate(tmp_stored_target_list(size(stored_target_list) + 1))
        tmp_stored_target_list(1:size(stored_target_list)) = stored_target_list(1:size(stored_target_list))
        tmp_stored_target_list(size(tmp_stored_target_list))%targets = targets
        deallocate(stored_target_list)
        call move_alloc(tmp_stored_target_list, stored_target_list)

        allocate(tmp_stored_masks(size(stored_masks) + 1))
        tmp_stored_masks(1:size(stored_masks)) = stored_masks(1:size(stored_masks))
        tmp_stored_masks(size(tmp_stored_masks)) = masks
        deallocate(stored_masks)
        call move_alloc(tmp_stored_masks, stored_masks)

        stored_subsets = [stored_subsets, current_subset%chars()]

      end block
    end if
  end subroutine


  !> @author Ronald Mclaren
  !> @date 2021-06-08
  !>
  !> @brief Finds the node inidices and other applicable data for a specific 
  !>        query string and returns the info in a target object.
  !>
  !> @param[in] lun - integer: Unit number for the BUFR file
  !> @param[in] current_subset - type(String): The current subset string
  !> @param[in] name - character(:): The name of the target
  !> @param[in] query_str - character(:): The query string
  !> @return targ - type(Target): The target
  !>
  type(Target) function find_target(lun, current_subset, name, query_str) result(targ)
    integer, intent(in) :: lun
    type(String), intent(in) :: current_subset
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: query_str

    integer, allocatable :: target_nodes(:)
    logical :: is_string = .false.
    integer :: node_idx, path_idx, rewind_idx
    integer :: index
    integer :: table_cursor, mnemonic_cursor
    character(len=10) :: subset
    character(len=10), allocatable :: mnemonics(:)
    integer, allocatable :: branches(:)
    logical :: is_missing_target
    type(IntList) :: seq_path
    character(len=:), allocatable :: dim_paths(:)
    integer, allocatable :: dim_idxs(:)

    call split_query_str(query_str, subset, mnemonics, index)

    is_missing_target = .false.
    ! Ignore targets with the wrong subset ID
    if (subset /= "*") then
      if (subset /= current_subset%chars()) then
        is_missing_target = .true.

        allocate(branches(0))
        allocate(target_nodes(0))
      end if
    end if

    if (.not. is_missing_target) then
      allocate(branches(size(mnemonics) - 1))
      allocate(target_nodes(0))

      seq_path = IntList()
      call seq_path%push(inode(lun))  ! Add root node id

      node_idx = 1
      table_cursor = 0
      mnemonic_cursor = 0
      do node_idx = inode(lun), isc(inode(lun))
        if (typ(node_idx) == Sequence .or. typ(node_idx) == Repeat .or. typ(node_idx) == StackedRepeat) then
          if (is_query_node(node_idx - 1)) then
            if (tag(node_idx) == mnemonics(mnemonic_cursor + 1) .and. &
                table_cursor == mnemonic_cursor) then

              mnemonic_cursor = mnemonic_cursor + 1
              branches(mnemonic_cursor) = node_idx - 1
            end if

            table_cursor = table_cursor + 1
          end if

          call seq_path%push(node_idx)

        else if (mnemonic_cursor == size(mnemonics) - 1 .and. &
                 table_cursor == mnemonic_cursor .and. &
                 tag(node_idx) == mnemonics(size(mnemonics))) then

          ! We found a target
          target_nodes = [target_nodes, node_idx]
          is_string = (itp(node_idx) == 3)

          call get_dim_info(branches, mnemonic_cursor, dim_paths, dim_idxs)
        end if

        if (seq_path%length() > 1) then
          ! Peak ahead to see if the next node is inside one of the containing sequences
          ! then go back up the approptiate number of sequences. You may have to exit several
          ! sequences in a row if the current sequence is the last element in the containing
          ! sequence.

          do path_idx = seq_path%length() - 1, 1, -1
            if (seq_path%at(path_idx) == jmpb(node_idx + 1)) then
              do rewind_idx = seq_path%length(), path_idx + 1, -1
                ! Exit the sequence

                ! If this is a query node, we need to update the query index.
                if (is_query_node(seq_path%at(rewind_idx) - 1)) then
                  if (mnemonic_cursor > 0 .and. table_cursor == mnemonic_cursor) then
                    mnemonic_cursor = mnemonic_cursor - 1
                  end if

                  table_cursor = table_cursor - 1
                end if

                ! Pop out of the current sequence
                call seq_path%pop()
              end do
              exit
            end if
          end do
        end if
      end do

      if (index > 0 .and. index <= size(target_nodes)) then
        if (index > size(target_nodes)) then
          call bort('Invalid index in query str ' // query_str // '.')
        end if

        target_nodes = [target_nodes(index)]
      end if

      if (size(target_nodes) > 1) then
        call bort('Query string must return 1 target. Are you missing an index? ' // query_str // '.')
      end if
    end if

    targ = Target(name, query_str, is_string, branches, target_nodes, null(), null())

    if (size(target_nodes) > 0) then
      allocate(targ%dim_paths, source=dim_paths)
      allocate(targ%export_dim_idxs, source=dim_idxs)
    else
      allocate(character(len=10)::targ%dim_paths(1))
      targ%dim_paths(1) = "*"
      allocate(targ%export_dim_idxs(1))
      targ%export_dim_idxs(1) = 1
    end if

    deallocate(mnemonics)
  end function  
  
  
  !> @author Ronald Mclaren
  !> @date 2021-09-06
  !>
  !> @brief Gets a list of strings for the sub-path to each dimension in the data.
  !>
  !> @param[in] branches - type(String)(:): The list of branch idxs to use
  !>
  subroutine get_dim_info(branches, mnemonic_cursor, dim_paths, dim_idxs)
    integer, intent(in) :: branches(:)
    integer, intent(in) :: mnemonic_cursor
    character(len=:), allocatable, intent(out) :: dim_paths(:)
    integer, allocatable, intent(out) :: dim_idxs(:)

    integer :: branch_idx
    integer :: dim_idx
    integer :: node_idx
    type(IntList), allocatable :: dim_path_node_idxs(:)
    type(String) :: current_dim_path
    character(len=10) :: mnemonic_str
    type(IntList) :: dim_idx_list

    dim_idx_list = IntList()

    ! Allocate enough int lists to accomadate the worst case scenario
    allocate(dim_path_node_idxs(mnemonic_cursor))

    ! Allocate enough memory to hold all the dim paths
    allocate(character(len=size(branches)*10 + size(branches) + 1) :: dim_paths(size(branches) + 1))
    dim_paths = " "

    dim_idx = 1
    current_dim_path = String("*")
    dim_paths(dim_idx)(1:len(current_dim_path%chars())) = current_dim_path%chars()
    call dim_idx_list%push(1)

    ! Split the branches into node idxs for each additional dimension
    if (mnemonic_cursor > 0) then
      do branch_idx = 1, mnemonic_cursor
        node_idx = branches(branch_idx)
        mnemonic_str = tag(node_idx)

        call current_dim_path%append(String("/" // mnemonic_str(2:len(trim(mnemonic_str)) - 1)))

        if (typ(node_idx) == DelayedRep .or. &
            typ(node_idx) == FixedRep .or. &
            typ(node_idx) == DelayedRepStacked) then

          dim_idx = dim_idx + 1
          call dim_idx_list%push(branch_idx + 1) ! +1 because of the root dimension
          dim_paths(dim_idx)(1:len(current_dim_path%chars())) = current_dim_path%chars()
        end if
      end do
    end if

    dim_paths = dim_paths(1:dim_idx)

    allocate(dim_idxs, source=dim_idx_list%array())
  end subroutine get_dim_info


  !> @author Ronald Mclaren
  !> @date 2021-06-08
  !>
  !> @brief Collects the data for the targets and adds them to the result set.
  !>
  !> @param[in] lun - integer: Unit number for the BUFR file
  !> @param[in] targets - type(Target)(:): Array of targets
  !> @param[in] result_set - type(ResultSet): The result set
  !>
  subroutine collect_data(lun, targets, masks, result_set)
    integer, intent(in) :: lun
    type(Target), target, intent(in) :: targets(:)
    type(ProcessingMasks), intent(in) :: masks
    type(ResultSet), intent(inout) :: result_set

    integer :: path_idx, target_idx
    integer :: node_idx, seq_node_idx, tmp_return_node_idx
    integer :: data_cursor
    integer :: return_node_idx
    type(DataField), pointer :: data_field
    type(DataFrame), pointer :: data_frame
    type(Target), pointer :: targ
    type(NodeValueTable) :: node_value_table
    type(RealList), pointer :: real_list
    type(IntList), pointer :: count_list, rep_list
    integer :: last_non_zero_return_idx
    type(IntList) :: current_path, current_path_returns


    current_path = IntList()
    current_path_returns = IntList()

    data_frame => result_set%next_data_frame()
    return_node_idx = -1

    ! Reorganize the data into a NodeValueTable to make lookups faster (avoid looping over all the data a bunch of
    ! times)
    node_value_table = NodeValueTable(inode(lun), isc(inode(lun)))
    do data_cursor = 1, nval(lun)
      node_idx = inv(data_cursor, lun)
      real_list => node_value_table%values_for_node(node_idx)

      if (masks%value_node_mask(node_idx)) then
        call real_list%push(val(data_cursor, lun))
      end if

      count_list => node_value_table%counts_for_node(node_idx)

      ! Unfortuantely the fixed replicated sequences do not store their counts as values for the Fixed Replication
      ! nodes. It's therefore necessary to discover this information by manually tracing the nested sequences and
      ! counting everything manually. Since we have to do it for fixed reps anyways, its easier just to do it for all
      ! the squences.

      if (jmpb(node_idx) > 0) then
        if (masks%path_node_mask(jmpb(node_idx))) then
          if ((typ(node_idx) == Sequence .and. &
                  (typ(jmpb(node_idx)) == DelayedBinary .or. typ(jmpb(node_idx)) == FixedRep)) .or. &
              typ(node_idx) == Repeat .or. &
              typ(node_idx) == StackedRepeat) then

            ! Add to the count of the currently active sequence
            count_list%at(count_list%length()) = count_list%at(count_list%length()) + 1
          end if
        end if
      end if

      if (current_path%length() >= 1) then
        if (node_idx == return_node_idx .or. &
            data_cursor == nval(lun) .or. &
            (current_path%length() > 1 .and. node_idx == current_path%at(current_path%length() - 1) + 1)) then
          ! Look for the first path return idx that is not 0 and check if its this node idx. Exit the sequence if its
          ! appropriate. A return idx of 0 indicates a sequence that occurs as the last element of another sequence.

          do path_idx = current_path_returns%length(), last_non_zero_return_idx, -1
            call current_path_returns%pop()
            call current_path%pop(seq_node_idx)

            ! Delayed and Stacked Reps are inconsistent with other sequence types and add an extra replication
            ! per sequence. We need to account for this here.
            if (typ(seq_node_idx) == DelayedRep .or. &
                typ(seq_node_idx) == DelayedRepStacked) then
              rep_list => node_value_table%counts_for_node(seq_node_idx + 1)
              rep_list%at(rep_list%length()) = rep_list%at(rep_list%length()) - 1
            end if
          end do

          last_non_zero_return_idx = current_path_returns%length()
          return_node_idx = current_path_returns%at(last_non_zero_return_idx)
        end if
      end if

      if (masks%path_node_mask(node_idx) .and. is_query_node(node_idx)) then
        if (typ(node_idx) == DelayedBinary .and. val(data_cursor, lun) == 0) then
          ! Ignore the node if it is a delayed binary and the value is 0
        else
          call current_path%push(node_idx)
          tmp_return_node_idx = link(node_idx)

          call current_path_returns%push(tmp_return_node_idx)

          if (tmp_return_node_idx /= 0) then
            last_non_zero_return_idx = current_path_returns%length()
            return_node_idx = tmp_return_node_idx
          else
            last_non_zero_return_idx = 1
            return_node_idx = 0

            if (data_cursor /= nval(lun)) then
              do path_idx = current_path%length(), 1, -1
                return_node_idx = link(jmpb(current_path%at(path_idx)))
                last_non_zero_return_idx = current_path_returns%length() - path_idx + 1

                if (return_node_idx /= 0) exit
              end do
            end if
          end if
        end if

        count_list => node_value_table%counts_for_node(node_idx + 1)
        call count_list%push(0)
      end if
    end do

    ! Uncomment to print the node value table
!    do node_idx = inode(lun), isc(inode(lun))
!      real_list = node_value_table%values_for_node(node_idx)
!      print *, tag(node_idx), real_list%array()
!    end do


    ! Uncomment to print he sequnce counts table
!    do node_idx = inode(lun), isc(inode(lun))
!      if (jmpb(node_idx) > 0) then
!        if (masks%path_node_mask(jmpb(node_idx))) then
!          count_list => node_value_table%counts_for_node(node_idx)
!          print *, " ",  tag(node_idx), "  size: ", count_list%length(), "  counts: ",  count_list%array()
!        end if
!      end if
!    end do

    do target_idx = 1, size(targets)
      targ => targets(target_idx)

      data_field => data_frame%data_fields(target_idx)
      data_field%name = String(targ%name)
      data_field%query_str = String(targ%query_str)
      data_field%is_string = targ%is_string
      allocate(data_field%dim_paths, source=targ%dim_paths)
      allocate(data_field%seq_path(size(targ%seq_path) + 1))
      data_field%seq_path(1) = 1
      data_field%seq_path(2:size(targ%seq_path)+1) = targ%seq_path
      allocate(data_field%export_dim_idxs, source=targ%export_dim_idxs)

      if (size(targ%node_ids) == 0) then
        ! Ignore targets where the required nodes could not be found in this subset
        allocate(data_field%data(1))
        data_field%data(1) = MissingValue
        data_field%missing = .true.
        allocate(data_field%seq_counts(1))
        data_field%seq_counts(1)%counts = [1]
      else
        allocate(data_field%seq_counts(size(targ%seq_path) + 1))
        data_field%seq_counts(1)%counts = [1]
        do path_idx = 1, size(targ%seq_path)
          count_list => node_value_table%counts_for_node(targ%seq_path(path_idx) + 1)
          allocate(data_field%seq_counts(path_idx + 1)%counts, source=count_list%array())
        end do

        real_list => node_value_table%values_for_node(targ%node_ids(1))
        allocate(data_field%data, source=real_list%array())

        if (size(data_field%data) == 0) data_field%missing = .true.
      end if
    end do

    call node_value_table%delete()
  end subroutine


  logical function is_query_node(node_idx)
    integer, intent(in) :: node_idx

    is_query_node = .false.
    if (typ(node_idx) == DelayedRep .or. &
        typ(node_idx) == FixedRep .or. &
        typ(node_idx) == DelayedRepStacked .or. &
        typ(node_idx) == DelayedBinary) then

      is_query_node = .true.
    end if
  end function


  !> NodeValueTable functions

  !> @author Ronald Mclaren
  !> @date 2021-11-22
  !>
  !> @brief Creates a new NodeValueTable instance.
  !>

  function init__node_value_table(start_node_idx, end_node_idx) result(node_value_table)
    integer, intent(in) :: start_node_idx
    integer, intent(in) :: end_node_idx
    type(NodeValueTable):: node_value_table

    integer :: idx

    node_value_table = NodeValueTable(start_node_idx, null(), null())
    allocate(node_value_table%value_lists(end_node_idx - start_node_idx + 1))
    allocate(node_value_table%count_lists(end_node_idx - start_node_idx + 1))

    do idx = 1, end_node_idx - start_node_idx + 1
      node_value_table%value_lists(idx) = RealList()
      node_value_table%count_lists(idx) = IntList()
    end do
  end function init__node_value_table


  !> @author Ronald Mclaren
  !> @date 2021-11-22
  !>
  !> @brief Get values for node with id.
  !>
  !> @param[in] self - class(NodeValueTable): The NodeValueTable instance
  !> @param[in] id - integer: The node id
  !>
  !> @returns type(RealList):: The NodeValueTable instance
  !>
  function node_value_table__values_for_node(self, id) result(values)
    class(NodeValueTable), target, intent(inout) :: self
    integer, intent(in) :: id
    type(RealList), pointer:: values

    values => self%value_lists(id - self%start_node_id + 1)
  end function node_value_table__values_for_node


  !> @author Ronald Mclaren
  !> @date 2021-11-22
  !>
  !> @brief Get values for node with id.
  !>
  !> @param[in] self - class(NodeValueTable): The NodeValueTable instance
  !> @param[in] id - integer: The node id
  !>
  !> @returns type(IntList):: The counts
  !>
  function node_value_table__counts_for_node(self, id) result(counts)
    class(NodeValueTable), target, intent(inout) :: self
    integer, intent(in) :: id
    type(IntList), pointer:: counts

    counts => self%count_lists(id - self%start_node_id + 1)
  end function node_value_table__counts_for_node


  !> @author Ronald Mclaren
  !> @date 2021-11-22
  !>
  !> @brief Delete the NodeValueTable instance.
  !>
  !> @param[in] self - class(NodeValueTable): The NodeValueTable instance
  !>
  subroutine node_value_table__delete(self)
    class(NodeValueTable), intent(inout) :: self

    integer :: idx

    if (allocated(self%value_lists)) then
      do idx = 1, size(self%value_lists)
        call self%value_lists(idx)%delete()
      end do

      deallocate(self%value_lists)
    end if

    if (allocated(self%count_lists)) then
      do idx = 1, size(self%count_lists)
        call self%count_lists(idx)%delete()
      end do

      deallocate(self%count_lists)
    end if
  end subroutine node_value_table__delete

end module modq_query
