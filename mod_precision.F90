module precision
   use, intrinsic :: ISO_FORTRAN_ENV

	private
	
	integer(kind=int32), public, parameter :: i16 = int16
	integer(kind=int32), public, parameter :: i32 = int32
	integer(kind=int32), public, parameter :: i64 = int64
	
	integer(kind=int32), public, parameter :: f32 = real32
	integer(kind=int32), public, parameter :: f64 = real64
	
	integer(kind=int32), public, parameter :: c32 = 2*f32
	integer(kind=int32), public, parameter :: c64 = 2*f64
	
	integer(kind=int32), public, parameter :: wl = LOGICAL_KINDS(1) ! 'working logical' 

end module precision
