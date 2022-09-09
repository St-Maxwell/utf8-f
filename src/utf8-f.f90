module utf8
    use utf8_detail
    implicit none
    private
    public :: utf8_string
    public :: utf8_string_iterator
    public :: construct_utf8_string
    public :: assignment(=)
    public :: utf8_len
    public :: utf8_at
    public :: utf8_slice
    public :: utf8_index
    public :: utf8_count
    public :: utf8_split
    public :: utf8_reverse
    public :: utf8_is_valid
end module utf8
