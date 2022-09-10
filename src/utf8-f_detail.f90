module utf8_detail
    use utf8_const
    implicit none
    public

    type :: utf8_string
        private
        character(len=:, kind=c_char), allocatable :: str
    contains
        procedure :: iterator
    end type

    type :: utf8_string_iterator
        private
        character(len=:, kind=c_char), pointer :: ptr => null()
        integer :: cur = 1
    contains
        procedure :: has_next => iterator_has_next
        procedure :: get_next => iterator_get_next
    end type

    interface construct_utf8_string
        module procedure :: utf8_construct_from_char
    end interface

    interface assignment(=)
        module procedure :: utf8_assign_from_char
        module procedure :: char_assign_from_utf8
    end interface

    interface utf8_is_valid
        module procedure :: utf8_is_valid_char
        module procedure :: utf8_is_valid_string
    end interface

contains

    subroutine utf8_construct_from_char(utf8, str, escape)
        type(utf8_string), intent(out) :: utf8
        character(len=*, kind=c_char), intent(in) :: str
        logical, optional, intent(in) :: escape
            !! if parse unicode escape sequence \uXXXX, default is .false.
        logical :: parse

        if (present(escape)) then
            parse = escape
        else
            parse = .false.
        end if

        if (parse) then
            ! to be implemented
            allocate (utf8%str, source=str)
        else
            allocate (utf8%str, source=str)
        end if

    end subroutine utf8_construct_from_char

    subroutine utf8_assign_from_char(utf8, str)
        type(utf8_string), intent(out) :: utf8
        character(len=*, kind=c_char), intent(in) :: str

        call utf8_construct_from_char(utf8, str, .false.)

    end subroutine utf8_assign_from_char

    subroutine char_assign_from_utf8(str, utf8)
        character(len=:, kind=c_char), allocatable, intent(out) :: str
        type(utf8_string), intent(in) :: utf8

        if (allocated(utf8%str)) then
            allocate (str, source=utf8%str)
        else
            allocate (character(len=0, kind=c_char) :: str)
        end if

    end subroutine char_assign_from_utf8

    !> return the number of UTF-8 code points
    pure function utf8_len(utf8) result(l)
        class(utf8_string), intent(in) :: utf8
        integer :: l
        integer :: i

        l = 0; i = 1
        do
            if (i > len(utf8%str)) exit
            i = i + codepoint_num_bytes(cast_byte(utf8%str(i:i)))
            l = l + 1
        end do

    end function utf8_len

    !> return the code point at specified position
    pure function utf8_at(utf8, idx) result(s)
        class(utf8_string), intent(in) :: utf8
        integer, intent(in) :: idx
        character(len=:, kind=c_char), allocatable :: s
        integer :: i, j, n

        if (idx < 1) then
            allocate (character(len=0, kind=c_char) :: s); return
        end if

        i = 1; j = 1
        do
            if (i > len(utf8%str)) then
                allocate (character(len=0, kind=c_char) :: s); return
            end if
            n = codepoint_num_bytes(cast_byte(utf8%str(i:i)))
            if (j == idx) then
                allocate (s, source=utf8%str(i:i + n - 1)); return
            end if
            i = i + n
            j = j + 1
        end do

    end function utf8_at

    !> return a substring of utf8_string
    pure function utf8_slice(utf8, begin, end) result(slice)
        class(utf8_string), intent(in) :: utf8
        integer, intent(in) :: begin
        integer, intent(in) :: end
        character(len=:, kind=c_char), allocatable :: slice
        integer :: bi, bj ! byte index for begin (i) and end (j)
        integer :: ci, cj ! codepoint index for begin (i) and end (j)
        integer :: n

        if (begin > end .or. begin > len(utf8%str) .or. end < 1) then
            allocate (character(len=0, kind=c_char) :: slice); return
        end if

        bi = 1; ci = 1
        if (begin > 1) then
            do
                if (ci == begin) exit
                if (bi > len(utf8%str)) then
                    allocate (character(len=0, kind=c_char) :: slice); return
                end if
                n = codepoint_num_bytes(cast_byte(utf8%str(bi:bi)))
                bi = bi + n
                ci = ci + 1
            end do
        end if

        bj = bi; cj = ci
        do
            if (bj > len(utf8%str)) then
                allocate (slice, source=utf8%str(bi:)); return
            end if
            n = codepoint_num_bytes(cast_byte(utf8%str(bi:bi)))
            if (cj == end) then
                allocate (slice, source=utf8%str(bi:bj + n - 1)); return
            end if
            bj = bj + n
            cj = cj + 1
        end do

    end function utf8_slice

    !> return the position where substring occurs in utf8_string for the first time
    pure function utf8_index(utf8, substring) result(idx)
        class(utf8_string), intent(in) :: utf8
        character(len=*, kind=c_char), intent(in) :: substring
        integer :: idx
        integer :: bit, cit
        integer :: nt, ls

        idx = 0
        bit = 1; cit = 1
        ls = len(substring)
        do
            if (bit + ls - 1 > len(utf8%str)) exit
            if (utf8%str(bit:bit + ls - 1) == substring(:)) then
                idx = cit; return
            end if
            nt = codepoint_num_bytes(cast_byte(utf8%str(bit:bit)))
            bit = bit + nt
            cit = cit + 1
        end do

    end function utf8_index

    !> count the substring in utf8_string
    !> overlaps are not considered
    !> e.g. utf8_count("AUAUAUAUAUAUAU","AUA") returns 3
    pure function utf8_count(utf8, substring) result(count)
        class(utf8_string), intent(in) :: utf8
        character(len=*, kind=c_char), intent(in) :: substring
        integer :: count

        if (.not. utf8_is_valid(substring) .or. len(substring) == 0) then
            count = 0; return
        end if

        count = count_internal(utf8%str, substring)

    contains

        pure recursive function count_internal(full, sub) result(c)
            character(len=*, kind=c_char), intent(in) :: full
            character(len=*, kind=c_char), intent(in) :: sub
            integer :: c
            integer :: l, idx

            l = len(sub)
            idx = index(full, sub)
            if (idx == 0) then
                c = 0; return
            else
                c = count_internal(full(idx + l:), sub) + 1
            end if

        end function count_internal

    end function utf8_count

    !> split utf8_string based on the separation string
    subroutine utf8_split(utf8, sep, list)
        class(utf8_string), intent(in), target :: utf8
        character(len=*, kind=c_char), intent(in) :: sep
        type(utf8_string), dimension(:), allocatable, intent(out) :: list
        character(len=:, kind=c_char), pointer :: ptr
        integer :: nsep, l
        integer :: i, e

        nsep = utf8_count(utf8, sep)
        allocate(list(nsep+1))

        if (nsep == 0) then
            allocate(list(1)%str, source=utf8%str)
        else
            l = len(sep)
            e = 1
            ptr => utf8%str(:)
            do i = 1, nsep
                e = index(ptr, sep)
                allocate(list(i)%str, source=ptr(:e-1))
                ptr => ptr(e+l:)
            end do
            allocate(list(nsep+1)%str, source=ptr(:))
        end if

    end subroutine utf8_split

    !> reverse the order of code points in place
    subroutine utf8_reverse(utf8)
        class(utf8_string), intent(inout) :: utf8
        character(len=:, kind=c_char), allocatable :: tmp
        integer :: i, j, l, n

        l = len(utf8%str)
        call move_alloc(from=utf8%str, to=tmp)
        allocate (character(len=l, kind=c_char) :: utf8%str)

        i = 1; j = l
        do
            if (i > l) exit
            n = codepoint_num_bytes(cast_byte(tmp(i:i)))
            utf8%str(j - n + 1:j) = tmp(i:i + n - 1)
            i = i + n
            j = j - n
        end do

    end subroutine utf8_reverse

    !> return an iterator of utf8_string
    function iterator(this) result(itr)
        class(utf8_string), target, intent(in) :: this
        type(utf8_string_iterator) :: itr

        itr%ptr => this%str

    end function iterator

!> iterator methods

    pure function iterator_has_next(this) result(r)
        class(utf8_string_iterator), intent(in) :: this
        logical :: r

        r = this%cur <= len(this%ptr)

    end function iterator_has_next

    function iterator_get_next(this) result(cp)
        class(utf8_string_iterator), intent(inout) :: this
        !character(len=:, kind=c_char), pointer :: cp
        character(len=:, kind=c_char), allocatable :: cp
        integer :: n

        n = codepoint_num_bytes(cast_byte(this%ptr(this%cur:this%cur)))
        cp = this%ptr(this%cur:this%cur + n - 1)
        this%cur = this%cur + n

    end function iterator_get_next

    !> check if the whole string is valid utf8 encoding
    pure function utf8_is_valid_char(str) result(r)
        character(len=*, kind=c_char), intent(in) :: str
        logical :: r
        integer(kind=c_int8_t) :: byte
        integer :: i, n

        i = 1
        do
            if (i > len(str)) exit
            byte = cast_byte(str(i:i))
            if (iand(byte, int(z'80', c_int8_t)) == int(z'00', c_int8_t)) then
                ! first byte: 0xxxxxxx and 00..7F
                i = i + 1
            else if (iand(byte, int(z'E0', c_int8_t)) == int(z'C0', c_int8_t) .and. &
                     iand(byte, int(z'1F', c_int8_t)) > int(z'01', c_int8_t)) then
                ! first byte: 110yyyyy and C2..DF
                if (i + 1 > len(str)) then
                    r = .false.; return
                end if
                byte = cast_byte(str(i + 1:i + 1))
                if (iand(byte, int(z'C0', c_int8_t)) /= int(z'80', c_int8_t)) then
                    ! secpnd byte: 10xxxxxx and 80..BF
                    r = .false.; return
                end if
                i = i + 2
            else if (iand(byte, int(z'F0', c_int8_t)) == int(z'E0', c_int8_t)) then
                ! first byte: 1110zzzz and E0..EF
                if (i + 2 > len(str)) then
                    r = .false.; return
                end if
                if (iand(cast_byte(str(i + 1:i + 1)), int(z'C0', c_int8_t)) /= int(z'80', c_int8_t) .or. &
                    iand(cast_byte(str(i + 2:i + 2)), int(z'C0', c_int8_t)) /= int(z'80', c_int8_t)) then
                    ! second and third bytes: 10xxxxxx
                    r = .false.; return
                end if
                if (byte == int(z'E0', c_int8_t)) then
                    if (iand(cast_byte(str(i + 1:i + 1)), int(z'3F', c_int8_t)) < int(z'20', c_int8_t)) then
                        ! E0  A0..BF  80..BF
                        r = .false.; return
                    end if
                end if
                if (byte == int(z'ED', c_int8_t)) then
                    if (iand(cast_byte(str(i + 1:i + 1)), int(z'3F', c_int8_t)) > int(z'1F', c_int8_t)) then
                        ! ED  80..9F  80..BF
                        r = .false.; return
                    end if
                end if
                i = i + 3
            else if (iand(byte, int(z'F8', c_int8_t)) == int(z'F0', c_int8_t) .and. &
                     iand(byte, int(z'07', c_int8_t)) < int(z'05', c_int8_t)) then
                ! first byte: 11110uuu and F0..F4
                if (i + 3 > len(str)) then
                    r = .false.; return
                end if
                if (iand(cast_byte(str(i + 1:i + 1)), int(z'C0', c_int8_t)) /= int(z'80', c_int8_t) .or. &
                    iand(cast_byte(str(i + 2:i + 2)), int(z'C0', c_int8_t)) /= int(z'80', c_int8_t) .or. &
                    iand(cast_byte(str(i + 3:i + 3)), int(z'C0', c_int8_t)) /= int(z'80', c_int8_t)) then
                    ! second, third, and last bytes: 10xxxxxx
                    r = .false.; return
                end if
                if (byte == int(z'F0', c_int8_t)) then
                    if (iand(cast_byte(str(i + 1:i + 1)), int(z'3F', c_int8_t)) < int(z'10', c_int8_t)) then
                        ! F0  90..BF  80..BF  80..BF
                        r = .false.; return
                    end if
                end if
                if (byte == int(z'F4', c_int8_t)) then
                    if (iand(cast_byte(str(i + 1:i + 1)), int(z'3F', c_int8_t)) > int(z'0F', c_int8_t)) then
                        ! F4  80..8F  80..BF  80..BF
                        r = .false.; return
                    end if
                end if
                i = i + 4
            else
                r = .false.; return
            end if
        end do

        r = .true.

    end function utf8_is_valid_char

    pure function utf8_is_valid_string(str) result(r)
        type(utf8_string), intent(in) :: str
        logical :: r

        r = utf8_is_valid_char(str%str)

    end function utf8_is_valid_string

!> private helper functions

    !> cast char to byte (8-bits integer in Fortran)
    !> display: private
    pure function cast_byte(char) result(byte)
        character(kind=c_char, len=1), intent(in) :: char
        integer(kind=c_int8_t) :: byte

        byte = transfer(char, byte)

    end function cast_byte

    !> get the number of bytes of a code point based on its first byte
    !> display: private
    pure function codepoint_num_bytes(byte) result(n)
        integer(kind=c_int8_t), intent(in) :: byte
        integer :: n

        n = NUM_BYTES_UTF8(iand(int(byte, 4), int(z'000000FF', 4)))
        if (n == 0) n = 1

    end function codepoint_num_bytes

end module utf8_detail
