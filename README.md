![](utf8-f.png)

---

utf8-f: Prototype of UTF-8 manipulation in Fortran.

## Desciption
The underlying data in a `utf8_string` object is a deferred-length string of `character(len=:, kind=c_char)` type. It is similar to many UTF-8 implementation in other programming languages, in which an array of `byte` or `char` is used to store the raw data. Just save the source files with UTF-8 encoding, then you can use utf8-f to get with UTF-8 encoded string. NOTE: I will never provide functions that convert UTF-8 to other encoding (e.g. UTF-16), because I don't like any of them.


## Example
```fortran
use utf8
implicit none
type(utf8_string) :: s

call construct_utf8_string(s, "Fortran さいこう")

write (*, "('s is a valid utf-8 string: ',g0)") utf8_valid(s)
write (*, "('The number of code points: ',g0)") utf8_len(s)
write (*, "('The 10th code point is ',3A)") "'", utf8_at(s,10), "'"

block !! iterate all code points
    type(utf8_string_iterator) :: it
    it = s%iterator()

    do while (it%has_next())
        write (*, "(3A)") "'", it%get_next(), "'"
    end do
end block
```

