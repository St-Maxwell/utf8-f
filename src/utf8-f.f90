module utf8
    use iso_c_binding, only: c_char, c_int8_t
    implicit none
    !private
    !public :: utf8_string

    type :: utf8_string
        !private
        character(len=:, kind=c_char), allocatable :: str
    contains
        procedure :: len => len_utf8_string
        procedure :: utf8_len => utf8_len_utf8_string
        procedure :: byte_at
        procedure :: utf8_at
        procedure :: utf8_slice
        procedure :: utf8_index
        procedure :: utf8_reverse
        procedure :: utf8_is_valid
        procedure :: iterator
    end type

    type :: utf8_string_iterator
        private
        character(len=:, kind=c_char), pointer :: ptr => null()
        !character(len=:, kind=c_char), pointer :: cur => null()
        integer :: cur = 1
    contains
        procedure :: has_next => iterator_has_next
        procedure :: get_next => iterator_get_next
    end type

    interface construct_utf8_string
        module procedure :: construct_utf8_string_from_string
    end interface

    !> refer to Table 3-7. Well-Formed UTF-8 Byte Sequences
    !> in http://www.unicode.org/versions/Unicode10.0.0/UnicodeStandard-10.0.pdf
    integer, dimension(0:255), parameter :: NUM_BYTES_UTF8 = [ &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x00..0x0F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x10..0x1F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x20..0x2F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x30..0x3F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x40..0x4F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x50..0x5F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x60..0x6F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x70..0x7F
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! 0x80..0x8F
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! 0x90..0x9F
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! 0xA0..0xAF
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! 0xB0..0xBF
        0, 0, & ! 0xC0..0xC1 - disallowed in UTF-8
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, & ! 0xC2..0xCF
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, & ! 0xD0..0xDF
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, & ! 0xE0..0xEF
        4, 4, 4, 4, 4, & ! 0xF0..0xF4
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 & ! 0xF5..0xFF - disallowed in UTF-8
        ]

contains

    subroutine construct_utf8_string_from_string(this, string)
        type(utf8_string), intent(out) :: this
        character(len=*), intent(in) :: string

        allocate (this%str, source=string)

    end subroutine construct_utf8_string_from_string

    !> return the number of raw bytes
    pure function len_utf8_string(this) result(l)
        class(utf8_string), intent(in) :: this
        integer :: l

        l = len(this%str)

    end function len_utf8_string

    !> return the number of UTF-8 code points
    pure function utf8_len_utf8_string(this) result(l)
        class(utf8_string), intent(in) :: this
        integer :: l
        integer :: i

        l = 0; i = 1
        do
            if (i > this%len()) exit
            i = i + codepoint_num_bytes(cast_byte(this%str(i:i)))
            l = l + 1
        end do

    end function utf8_len_utf8_string

    !> return the byte at specified position
    pure function byte_at(this, idx) result(s)
        class(utf8_string), intent(in) :: this
        integer, intent(in) :: idx
        character(len=1, kind=c_char) :: s

        s = this%str(idx:idx)

    end function byte_at

    !> return the code point at specified position
    pure function utf8_at(this, idx) result(s)
        class(utf8_string), intent(in) :: this
        integer, intent(in) :: idx
        character(len=:, kind=c_char), allocatable :: s
        integer :: i, j, n

        if (idx < 1) then
            allocate (character(len=0, kind=c_char) :: s); return
        end if

        i = 1; j = 1
        do
            if (i > this%len()) then
                allocate (character(len=0, kind=c_char) :: s); return
            end if
            n = codepoint_num_bytes(cast_byte(this%str(i:i)))
            if (j == idx) then
                allocate (s, source=this%str(i:i + n - 1)); return
            end if
            i = i + n
            j = j + 1
        end do

    end function utf8_at

    pure function utf8_slice(this, begin, end) result(slice)
        class(utf8_string), intent(in) :: this
        integer, intent(in) :: begin
        integer, intent(in) :: end
        character(len=:, kind=c_char), allocatable :: slice
        integer :: bi, bj ! byte index for begin (i) and end (j)
        integer :: ci, cj ! codepoint index for begin (i) and end (j)
        integer :: n

        if (begin > end .or. begin > this%len() .or. end < 1) then
            allocate (character(len=0, kind=c_char) :: slice); return
        end if

        bi = 1; ci = 1
        if (begin > 1) then
            do
                if (ci == begin) exit
                if (bi > this%len()) then
                    allocate (character(len=0, kind=c_char) :: slice); return
                end if
                n = codepoint_num_bytes(cast_byte(this%str(bi:bi)))
                bi = bi + n
                ci = ci + 1
            end do
        end if

        bj = bi; cj = ci
        do
            if (bj > this%len()) then
                allocate (slice, source=this%str(bi:)); return
            end if
            n = codepoint_num_bytes(cast_byte(this%str(bi:bi)))
            if (cj == end) then
                allocate (slice, source=this%str(bi:bj + n - 1)); return
            end if
            bj = bj + n
            cj = cj + 1
        end do

    end function utf8_slice

    pure function utf8_index(this, substring) result(idx)
        class(utf8_string), intent(in) :: this
        character(len=*, kind=c_char), intent(in) :: substring
        integer :: idx
        integer :: bit, cit
        integer :: nt, ls

        idx = 0
        bit = 1; cit = 1
        ls = len(substring)
        do
            if (bit + ls - 1 > this%len()) exit
            if (this%str(bit:bit+ls-1) == substring(:)) then
                idx = cit; return
            end if
            nt = codepoint_num_bytes(cast_byte(this%str(bit:bit)))
            bit = bit + nt
            cit = cit + 1
        end do

    end function utf8_index

    subroutine utf8_reverse(this)
        class(utf8_string), intent(inout) :: this
        character(len=:, kind=c_char), allocatable :: tmp
        integer :: i, j, l, n

        l = this%len()
        call move_alloc(from=this%str, to=tmp)
        allocate (character(len=l, kind=c_char) :: this%str)

        i = 1; j = l
        do
            if (i > l) exit
            n = codepoint_num_bytes(cast_byte(tmp(i:i)))
            this%str(j - n + 1:j) = tmp(i:i + n - 1)
            i = i + n
            j = j - n
        end do

    end subroutine utf8_reverse

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

    pure function utf8_is_valid(this) result(r)
        class(utf8_string), intent(in) :: this
        logical :: r
        integer(kind=c_int8_t) :: byte
        integer :: i, n

        i = 1
        do
            if (i > this%len()) exit
            byte = cast_byte(this%str(i:i))
            if (iand(byte,int(z'80',c_int8_t)) == int(z'00',c_int8_t)) then
                ! first byte: 0xxxxxxx and 00..7F
                i = i + 1
            else if (iand(byte,int(z'E0',c_int8_t)) == int(z'C0',c_int8_t) .and. &
                     iand(byte,int(z'1F',c_int8_t)) > int(z'01',c_int8_t)) then
                ! first byte: 110yyyyy and C2..DF
                if (i+1 > this%len()) then
                    r = .false.; return
                end if
                byte = cast_byte(this%str(i+1:i+1))
                if (iand(byte,int(z'C0',c_int8_t)) /= int(z'80',c_int8_t)) then
                    ! secpnd byte: 10xxxxxx and 80..BF
                    r = .false.; return
                end if
                i = i + 2
            else if (iand(byte,int(z'F0',c_int8_t)) == int(z'E0',c_int8_t)) then
                ! first byte: 1110zzzz and E0..EF
                if (i+2 > this%len()) then
                    r = .false.; return
                end if
                if (iand(cast_byte(this%str(i+1:i+1)),int(z'C0',c_int8_t)) /= int(z'80',c_int8_t) .or. &
                    iand(cast_byte(this%str(i+2:i+2)),int(z'C0',c_int8_t)) /= int(z'80',c_int8_t)) then
                    ! second and third bytes: 10xxxxxx
                    r = .false.; return
                end if
                if (byte == int(z'E0',c_int8_t)) then
                    if (iand(cast_byte(this%str(i+1:i+1)),int(z'3F',c_int8_t)) < int(z'20',c_int8_t)) then
                        ! E0  A0..BF  80..BF
                        r = .false.; return
                    end if
                end if
                if (byte == int(z'ED',c_int8_t)) then
                    if (iand(cast_byte(this%str(i+1:i+1)),int(z'3F',c_int8_t)) > int(z'1F',c_int8_t)) then
                        ! ED  80..9F  80..BF
                        r = .false.; return
                    end if
                end if
                i = i + 3
            else if (iand(byte,int(z'F8',c_int8_t)) == int(z'F0',c_int8_t) .and. &
                     iand(byte,int(z'07',c_int8_t)) < int(z'05',c_int8_t)) then
                ! first byte: 11110uuu and F0..F4
                if (i+3 > this%len()) then
                    r = .false.; return
                end if
                if (iand(cast_byte(this%str(i+1:i+1)),int(z'C0',c_int8_t)) /= int(z'80',c_int8_t) .or. &
                    iand(cast_byte(this%str(i+2:i+2)),int(z'C0',c_int8_t)) /= int(z'80',c_int8_t) .or. &
                    iand(cast_byte(this%str(i+3:i+3)),int(z'C0',c_int8_t)) /= int(z'80',c_int8_t)) then
                    ! second, third, and last bytes: 10xxxxxx
                    r = .false.; return
                end if
                if (byte == int(z'F0',c_int8_t)) then
                    if (iand(cast_byte(this%str(i+1:i+1)),int(z'3F',c_int8_t)) < int(z'10',c_int8_t)) then
                        ! F0  90..BF  80..BF  80..BF
                        r = .false.; return
                    end if
                end if
                if (byte == int(z'F4',c_int8_t)) then
                    if (iand(cast_byte(this%str(i+1:i+1)),int(z'3F',c_int8_t)) > int(z'0F',c_int8_t)) then
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

    end function utf8_is_valid

!> private helper function

    !> cast char to byte (8-bits integer in Fortran)
    pure function cast_byte(char) result(byte)
        character(kind=c_char, len=1), intent(in) :: char
        integer(kind=c_int8_t) :: byte

        byte = transfer(char, byte)

    end function cast_byte

    !> get the number of bytes of a code point based on its first byte
    pure function codepoint_num_bytes(byte) result(n)
        integer(kind=c_int8_t), intent(in) :: byte
        integer :: n

        n = NUM_BYTES_UTF8(iand(int(byte, 4), int(z'000000FF', 4)))
        if (n == 0) n = 1

    end function codepoint_num_bytes

end module utf8
