
module modq_base_types
    type, abstract :: ComparableObject
    contains
        procedure(equals_interface), pass(self), deferred :: equals_obj
    end type ComparableObject

    abstract interface
        logical function equals_interface(self, other)
            import
            class(ComparableObject), intent(in) :: self
            class(ComparableObject), intent(in) :: other
        end function
    end interface


    type, abstract, extends(ComparableObject):: HashableObject
    contains
        procedure(hash_interface), pass(self), deferred :: hash
    end type HashableObject

    abstract interface
        integer function hash_interface(self)
            import
            class(HashableObject), intent(in) :: self
        end function
    end interface
end module modq_base_types