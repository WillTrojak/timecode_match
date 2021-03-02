module match
   use precision
   implicit none

   private

   public :: array_match_ndi
   public :: b_start

   integer(kind=i32), parameter :: b_div_i32_default = 8_i32
   integer(kind=i64), parameter :: b_div_i64_default = 8_i64

   ! This will pick one of the functions dependent on the input types
   interface array_match_ndi
      module procedure array_match_ndi_i32, array_match_ndi_i64
   end interface array_match_ndi

   
   ! This will pick one of the functions dependent on the input types
   ! * TODO - b_start_{T}_array where a0 is an array, this would be quuicker
   interface b_start
      module procedure b_start_i32_single, b_start_i64_single
   end interface b_start
   
contains

   function array_match_ndi_i32(a, b, di_max) result(di_count)

      integer(kind=i32), intent(in) :: a(:), b(:)

      integer(kind=i32), intent(in) :: di_max

      integer(kind=i32) :: di_count(di_max)

      integer(kind=i32) :: n_a, n_b
      integeR(kind=i32) :: di, di_1, di_2, i, j, current_idx
      
      
      n_a = size(a)
      n_b = size(b)
      
      current_idx = 1_i32
      di_count = 0_i32
      
      do i=1_i32,n_a-1
         do j=current_idx,n_b
            di_1 = b(j) - a(i)
            if(di_1 .lt. di_max .and. di_1 .ge. 0_i32)then
               di_2 = b(j) - a(i+1)
               
               di = merge(di_2, di_1, di_2 .ge. 0_i32) + 1
               current_idx = merge(i+1, i, di_2 .ge. 0_i32)
               di_count(di) = di_count(di) + 1
               exit
            endif
         enddo
      enddo
      
      do j=current_idx,size(b)
         di = b(j) - a(n_a)
         if(di .lt. di_max)then
            di_count(di + 1) = di_count(di + 1) + 1
         endif
      enddo
      
   end function array_match_ndi_i32

   
   function array_match_ndi_i64(a, b, di_max) result(di_count)

      integer(kind=i64), intent(in) :: a(:), b(:)

      integer(kind=i64), intent(in) :: di_max

      integer(kind=i64) :: di_count(di_max)

      integer(kind=i64) :: n_a, n_b
      integeR(kind=i64) :: di, di_1, di_2, i, j, current_idx
      
      
      n_a = size(a)
      n_b = size(b)
      
      current_idx = 1_i64
      di_count = 0_i64
      
      do i=1_i64,n_a-1
         do j=current_idx,n_b
            di_1 = b(j) - a(i)
            if(di_1 .lt. di_max .and. di_1 .ge. 0_i64)then
               di_2 = b(j) - a(i+1)
               
               di = merge(di_2, di_1, di_2 .ge. 0_i64) + 1
               current_idx = merge(i+1, i, di_2 .ge. 0_i64)
               di_count(di) = di_count(di) + 1
               exit
            endif
         enddo
      enddo
      
      do j=current_idx,size(b)
         di = b(j) - a(n_a)
         if(di .lt. di_max)then
            di_count(di + 1) = di_count(di + 1) + 1
         endif
      enddo
      
   end function array_match_ndi_i64

   ! Assumes a0 \in [b(1), b(n)]
   function b_start_i32_single(a0, b, n_div) result(b_start)

      integer(kind=i32), intent(in) :: a0, b(:)
      
      integer(kind=i32), optional, intent(in) :: n_div

      integer(kind=i32) :: b_start

      integer(kind=i32) :: n_div_max, n_b, i
      integer(kind=i32) :: b0, b1, b2

      n_b = int(size(b), kind=i32)
      
      n_div_max = b_div_i32_default
      if(present(n_div)) n_div_max = n_div
      if(a0 .eq. b(1)) then
         b_start = 1_i32
         return
      elseif(a0 .ge. b(n_b))then
         b_start = n_b
         return
      end if

      b0 = b(1)
      b1 = b(n_b)
      do i=1,n_div_max
         b2 = (b0 + b1)/2_i32
         if(b2 .eq. a0)then
            b_start = b2
            exit
         elseif(b2 .gt. a0)then
            b1 = b2
         else
            b0 = b2
         endif            
      enddo
         
   end function b_start_i32_single

   
   ! Assumes a0 \in [b(1), b(n)]
   function b_start_i64_single(a0, b, n_div) result(b_start)

      integer(kind=i64), intent(in) :: a0, b(:)
      
      integer(kind=i64), optional, intent(in) :: n_div

      integer(kind=i64) :: b_start

      integer(kind=i64) :: n_div_max, n_b, i
      integer(kind=i64) :: b0, b1, b2

      n_b = int(size(b), kind=i64)
      
      n_div_max = b_div_i64_default
      if(present(n_div)) n_div_max = n_div
      
      if(a0 .eq. b(1)) then
         b_start = 1_i64
         return
      elseif(a0 .ge. b(n_b))then
         b_start = n_b
         return
      end if

      b0 = b(1)
      b1 = b(n_b)
      do i=1,n_div_max
         b2 = (b0 + b1)/2_i64
         if(b2 .eq. a0)then
            b_start = b2
            exit
         elseif(b2 .gt. a0)then
            b1 = b2
         else
            b0 = b2
         endif            
      enddo
         
   end function b_start_i64_single

   
end module match
