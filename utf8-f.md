---
project: utf8-f
summary: Prototype of UTF-8 manipulation in Fortran.
project_github: https://github.com/St-Maxwell/utf8-f
author: St Maxwell
github: https://github.com/St-Maxwell
src_dir: ./src
output_dir: ./docs
exclude_dir: ./test
             ./app
docmark: <
predocmark: >
source: true
sort: alpha
print_creation_date: true
extra_mods: iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---

Prototype of UTF-8 manipulation in Fortran.


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
