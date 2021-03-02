program simple_test
   use match
   use precision
  
   integer(kind=i32), parameter :: di_max = 3
  
   integer(kind=i32), allocatable :: a(:), b(:)

   integer(kind=i32) :: di_count(di_max)

   a = [1, 3, 7, 10, 13]
   b = [1, 4, 9, 14, 16, 19] 

   di_count = array_match_ndi(a, b, di_max)

   print *,di_count
   
end program simple_test
