Module algebras

!********************************************************************************
!***LAST MODIFICATION: 13/11/09 (DDMMYY)
!***DESCRIPTION: This module contains the definition of a type that it's a general container for real or complex numbers. 
!			It is necessary to define the operations of the container (matmul...)
!***USE:	Module Kinds for common type definitions.
!	CONTAINS:
!		- Private : Type definition 'construc'. Includes
!			PROCEDURES
!			- new1 (nopass): initialize type 'mgen' with the proper type
!		        	- Interfaz
!					- IN - character(len=*) - tipo - label with the type of data (only 'real' implemented)
!					- IN - integer - ind(:) - array containing the dimension 
!					- OUT - class(mgen) - mgen1 - class allocatable able to contain several type of data
!			- newr1 (nopass): initialize type 'mgen' as mgenr1 and introduce data
!		        	- Interfaz
!					- IN - real - ind(:) - array containing the data
!					- OUT - class(mgen) - mgen1 - class allocatable able to contain several type of data
!			- newr2 (nopass): initialize type 'mgen' as mgenr2 and introduce data
!		        	- Interfaz
!					- IN - real - ind(:,:) - array containing the data
!					- OUT - class(mgen) - mgen1 - class allocatable able to contain several type of data
!			GENERIC INTERFACES
!			- new = > new1, newr1, newr2

!		- Public : Type definition 'operador'. Includes
!			PROCEDURES
!			- dot: Multiply procedures for mgen types. Currently only supports reals containers
!		        	- Interfaz
!					- IN - class(mgen) - mgen1,mgen2 - variables containing the arrays for multiplying
!					- OUT - class(mgen) - mgen3- class allocatable containing the result of multiplying

!		- Public : Type definition 'mgen'. Includes
!			DATA
!			- Public : constructor - type 'construc' for accessing to its 'new' procedure
!			- Public : ind(:) - allocatable integer array containing the number of elements in every dimension
!			PROCEDURES
!			- put1: Dummy function. Error message
!			- put2: Dummy function. Error message
!			- putv1: Dummy function. Error message
!			- putv2: Dummy function. Error message
!			- in1: Dummy function. Error message
!			- out1: Dummy function. Error message
!			- in2: Dummy function. Error message
!			- out2: Dummy function. Error message
!			- print1: Dummy function. Error message
!			GENERIC INTERFACES
!			- put = > put1, put2
!			- putv = > putv1, putv2
!			- in = > in1, in2
!			- out = > out1, out2
!			- print = > print1		        	

!		- Private : Type definition 'mgenr1'. Extension of 'mgen'.
!			DATA
!			- Private: n(:) - allocatable real array containing the data
!			PROCEDURES
!			- put1(putr1): Introduce data (one real variable) in a specific position
!		        	- Interfaz
!					- IN - real - dato1 - real variable with data
!					- IN - integer - map - position of the data in the array of mgenr1
!					- INOUT - class(mgenr1) - mgen1 - class containing a real array
!			- putv1(putvr1): Introduce data (one real array) in a specific position
!		        	- Interfaz
!					- IN - real - dato1(:) - real array with data
!					- IN - integer - map - insertion position of the array
!					- INOUT - class(mgenr1) - mgen1 - class containing a real array
!			- in1(inr1): Introduce data (one real array) inside the container. The dimension of the array
!				      should be the same as the dimension of the array inside the container
!		        	- Interfaz
!					- IN - real - dato1(:) - real array with data
!					- INOUT - class(mgenr1) - mgen1 - class containing a real array
!			- out1(outr1): Pass the data from the container to an array
!		        	- Interfaz
!					- IN - class(mgenr1) - mgen1 - container with the data.
!					- OUT - real - dato1(:) - pointer to the data
!			- print1(printr1): Print the data contained in 
!		        	- Interfaz
!					- IN - class(mgenr1) - mgen1 - container with the data.

!		- Private : Type definition 'mgenr2'. Extension of 'mgen'.
!			DATA
!			- Private: n(:,:) - allocatable real matrix containing the data
!			PROCEDURES
!			- put2(putr2): Introduce data (one real variable) in a specific position
!		        	- Interfaz
!					- IN - real - dato1 - real variable with data
!					- IN - integer - map(2) - position of the data in the matrix of mgenr2
!					- INOUT - class(mgenr2) - mgen1 - class containing a real array
!			- putv2(putvr2): Introduce data (one real matrix) in a specific position
!		        	- Interfaz
!					- IN - real - dato1(:,:) - real array with data
!					- IN - integer - map(2) - insertion position of the array
!					- INOUT - class(mgenr1) - mgen1 - class containing a real array
!			- in2(inr2): Introduce data (one real matrix) inside the container. The dimension of the matrix
!				      should be the same as the dimension of the matrix inside the container
!		        	- Interfaz
!					- IN - real - dato1(:,:) - real array with data
!					- INOUT - class(mgenr2) - mgen1 - class containing a real matrix
!			- out2(outr2): Pass the data from the container to an array
!		        	- Interfaz
!					- IN - class(mgenr2) - mgen1 - container with the data.
!					- OUT - real - dato1(:,:) - pointer to the data
!			- print2(printr2): Print the data contained in 
!		        	- Interfaz
!					- IN - class(mgenr2) - mgen1 - container with the data.
!********************************************************************************
use kinds

implicit none

private

public mgen
!********************************************************************************
!**************** DECLARATIONS **************************************************
!********************************************************************************	
	type				:: construc	
	contains
		procedure,nopass			:: new1
		procedure,nopass			:: newr1
		procedure,nopass			:: newr2
		procedure,nopass			:: newc1
		procedure,nopass			:: newc2
		generic					:: new=>new1,newr1,newr2,newc1,newc2
	end type construc
!********************************************************************************
	type mgen
		type(construc)				:: constructor
		integer(kind=k9),allocatable		:: ind(:)
	contains
		procedure				:: put1
		procedure				:: put2
		procedure				:: put3
		procedure				:: put4
		procedure				:: putv1
		procedure				:: putv2
		procedure				:: putv3
		procedure				:: putv4
		procedure				:: in1
		procedure				:: in2
		procedure				:: in3
		procedure				:: in4
		procedure				:: out1
		procedure				:: out2
		procedure				:: out3
		procedure				:: out4
		procedure				:: print1
		procedure,nopass			:: tipo => tipo
		generic					:: put=>put1,put2,put3,put4
		generic					:: putv=>putv1,putv2,putv3,putv4
		generic					:: in=>in1,in2,in3,in4
		generic					:: out=>out1,out2,out3,out4
		generic					:: print=>print1
	end type mgen
!********************************************************************************
	type,extends(mgen)			:: mgenr1
		real( kind = dp ),allocatable,private	:: n(:)
	contains
		procedure				:: put1 => putr1
		procedure				:: putv1 => putvr1
		procedure				:: in1 => inr1
		procedure				:: out1 => outr1
		procedure				:: print1 => printr1
		procedure,nopass			:: tipo => tipor1
	end type mgenr1
!********************************************************************************
	type,extends(mgen)			:: mgenr2
		real( kind = dp ),allocatable,private	:: n(:,:)
	contains
		procedure				:: put2 => putr2
		procedure				:: putv2 => putvr2
		procedure				:: in2 => inr2
		procedure				:: out2 => outr2
		procedure				:: print1 => printr2
		procedure,nopass			:: tipo => tipor2
	end type mgenr2
!********************************************************************************
	type,extends(mgen)			:: mgenc1
		complex( kind = dp ),allocatable,private	:: n(:)
	contains
		procedure				:: put3 => putc1
		procedure				:: putv3 => putvc1
		procedure				:: in3 => inc1
		procedure				:: out3 => outc1
		procedure				:: print1 => printc1
		procedure,nopass			:: tipo => tipoc1
	end type mgenc1
!********************************************************************************
	type,extends(mgen)			:: mgenc2
		complex( kind = dp ),allocatable,private	:: n(:,:)
	contains
		procedure				:: put4 => putc2
		procedure				:: putv4 => putvc2
		procedure				:: in4 => inc2
		procedure				:: out4 => outc2
		procedure				:: print1 => printc2
		procedure,nopass			:: tipo => tipoc2
	end type mgenc2
!********************************************************************************
contains
!********************************************************************************
!**************** TYPE CONSTRUC**************************************************
!********************************************************************************
	subroutine new1 (mgen1,tipo,ind)

		implicit none

		class(mgen),allocatable,intent(out)				:: mgen1
		character(len=*),intent(in)		 			:: tipo

		integer(kind=k9)						:: ind(:)

		select case (tipo)
			case('real')
				select case (size(ind))
					case(1)
						allocate(mgenr1::mgen1)
					case(2)
						allocate(mgenr2::mgen1)
					case default
						print*,'module kinds, subroutine new'
						print*,'Tipo de dato aun no implementado'
						stop
				end select
			case('complex')
				select case (size(ind))
					case(1)
						allocate(mgenc1::mgen1)
					case(2)
						allocate(mgenc2::mgen1)
					case default
						print*,'module kinds, subroutine new'
						print*,'Tipo de dato aun no implementado'
						stop
				end select
			case default
				print*,'module kinds, subroutine new'
				print*,'Tipo de dato aun no implementado'
				stop
		end select

		allocate (mgen1%ind(size(ind)))

		mgen1%ind=ind

		select type (mgen1)
			type is (mgenr1)
				allocate (mgen1%n(mgen1%ind(1)))
				mgen1%n=0.0_dp
			type is (mgenr2)
				allocate (mgen1%n(mgen1%ind(1),mgen1%ind(2)))
				mgen1%n=0.0_dp
			type is (mgenc1)
				allocate (mgen1%n(mgen1%ind(1)))
				mgen1%n=0.0_dp
			type is (mgenc2)
				allocate (mgen1%n(mgen1%ind(1),mgen1%ind(2)))
				mgen1%n=0.0_dp
			class default
				print*,'module kinds, subroutine new'
				print*,'Tipo de dato aun no implementado'
				stop
		end select


		return
			
	end subroutine new1

	subroutine newr1 (mgen1,dato1)

		implicit none

		class(mgen),allocatable,intent(out)				:: mgen1
		real( kind = dp ),intent(in)					:: dato1(:)

		allocate(mgenr1::mgen1)

		allocate (mgen1%ind(1))

		mgen1%ind(1)=size(dato1)

		select type (mgen1)
			type is (mgenr1)
				allocate (mgen1%n(mgen1%ind(1)))
				mgen1%n=dato1
		end select

		return
			
	end subroutine newr1

	subroutine newr2 (mgen1,dato1)

		implicit none

		class(mgen),allocatable,intent(out)				:: mgen1
		real( kind = dp ),intent(in)					:: dato1(:,:)

		allocate(mgenr2::mgen1)

		allocate (mgen1%ind(2))

		mgen1%ind(1)=size(dato1,1)
		mgen1%ind(2)=size(dato1,2)

		select type (mgen1)
			type is (mgenr2)
				allocate (mgen1%n(mgen1%ind(1),mgen1%ind(2)))
				mgen1%n=dato1
		end select

		return
			
	end subroutine newr2

	subroutine newc1 (mgen1,dato1)

		implicit none

		class(mgen),allocatable,intent(out)				:: mgen1
		complex( kind = dp ),intent(in)					:: dato1(:)

		allocate(mgenc1::mgen1)

		allocate (mgen1%ind(1))

		mgen1%ind(1)=size(dato1)

		select type (mgen1)
			type is (mgenc1)
				allocate (mgen1%n(mgen1%ind(1)))
				mgen1%n=dato1
		end select

		return
			
	end subroutine newc1

	subroutine newc2 (mgen1,dato1)

		implicit none

		class(mgen),allocatable,intent(out)				:: mgen1
		complex( kind = dp ),intent(in)					:: dato1(:,:)

		allocate(mgenc2::mgen1)

		allocate (mgen1%ind(2))

		mgen1%ind(1)=size(dato1,1)
		mgen1%ind(2)=size(dato1,2)

		select type (mgen1)
			type is (mgenc2)
				allocate (mgen1%n(mgen1%ind(1),mgen1%ind(2)))
				mgen1%n=dato1
		end select

		return
			
	end subroutine newc2
!********************************************************************************
!**************** TYPE MGENR1****************************************************
!********************************************************************************
	subroutine putr1 (mgen1,dato1,map)

		implicit none

		class(mgenr1),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1
		integer( kind = k9 ),intent(in)				:: map

		mgen1%n(map)=mgen1%n(map)+dato1

		return
			
	end subroutine putr1

	subroutine putvr1 (mgen1,dato1,map)

		implicit none

		class(mgenr1),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:)
		integer( kind = k9 ),intent(in)				:: map

		mgen1%n(map:map+size(dato1)-1_k9)=mgen1%n(map:map+size(dato1)-1_k9)+dato1

		return
			
	end subroutine putvr1

	subroutine inr1 (mgen1,dato1)

		implicit none

		class(mgenr1),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:)

		mgen1%n=mgen1%n+dato1

		return
			
	end subroutine inr1

	subroutine outr1 (mgen1,dato1)

		implicit none

		class(mgenr1),target,intent(in)				:: mgen1
		real( kind = dp ),pointer,intent(out)			:: dato1(:)

		allocate(dato1(mgen1%ind(1)))
		dato1=>mgen1%n

		return
			
	end subroutine outr1

	subroutine  printr1(mgen1)

		implicit none

		class(mgenr1),intent(in)				:: mgen1

		print*, mgen1%n

		return

	end subroutine printr1

	function tipor1 ()

		implicit none
		character ( len=20 )					:: tipor1
		
		tipor1 = 'mgenr1'

		return
			
	end function tipor1
!********************************************************************************
!**************** TYPE MGENR2****************************************************
!********************************************************************************
	subroutine putr2 (mgen1,dato1,map)

		implicit none

		class(mgenr2),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1
		integer( kind = k9 ),intent(in)				:: map(2)

		mgen1%n(map(1),map(2))=mgen1%n(map(1),map(2))+dato1

		return
			
	end subroutine putr2

	subroutine putfr2 (mgen1,dato1,map)

		implicit none

		class(mgenr2),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:)
		integer( kind = k9 ),intent(in)				:: map(2)

		mgen1%n(map(1):map(1)+size(dato1)-1_k9,map(2))=mgen1%n(map(1):map(1)+size(dato1,1)-1_k9,map(2))+dato1

		return
			
	end subroutine putfr2

	subroutine putvr2 (mgen1,dato1,map)

		implicit none

		class(mgenr2),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:,:)
		integer( kind = k9 ),intent(in)				:: map(2)

		mgen1%n(map(1):map(1)+size(dato1,1)-1_k9,map(2):map(2)+size(dato1,2)-1_k9)=&
		mgen1%n(map(1):map(1)+size(dato1,1)-1_k9,map(2):map(2)+size(dato1,2)-1_k9)+dato1

		return
			
	end subroutine putvr2

	subroutine inr2 (mgen1,dato1)

		implicit none

		class(mgenr2),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:,:)

		mgen1%n=mgen1%n+dato1

		return
			
	end subroutine inr2

	subroutine outr2 (mgen1,dato1)

		implicit none

		class(mgenr2),target,intent(in)				:: mgen1
		real( kind = dp ),pointer,intent(out)			:: dato1(:,:)

		call outr2in(mgen1%n,dato1)

		return
			
	end subroutine outr2

	subroutine outr2in (mgen1,dato1)

		implicit none

		real( kind = dp ),target,intent(in)			:: mgen1(:,:)
		real( kind = dp ),pointer,intent(out)			:: dato1(:,:)

		allocate(dato1(size(mgen1,1),size(mgen1,2)))
		dato1=>mgen1

		return
			
	end subroutine outr2in

	subroutine  printr2(mgen1)

		implicit none

		class(mgenr2),intent(in)				:: mgen1

		print*, mgen1%n

		return

	end subroutine printr2

	function tipor2 ()

		implicit none
		character ( len=20 )					:: tipor2
		
		tipor2 = 'mgenr2'

		return
			
	end function tipor2
!********************************************************************************
!**************** TYPE MGENC1****************************************************
!********************************************************************************
	subroutine putc1 (mgen1,dato1,map)

		implicit none

		class(mgenc1),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1
		integer( kind = k9 ),intent(in)				:: map

		mgen1%n(map)=mgen1%n(map)+dato1

		return
			
	end subroutine putc1

	subroutine putvc1 (mgen1,dato1,map)

		implicit none

		class(mgenc1),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:)
		integer( kind = k9 ),intent(in)				:: map

		mgen1%n(map:map+size(dato1)-1_k9)=mgen1%n(map:map+size(dato1)-1_k9)+dato1

		return
			
	end subroutine putvc1

	subroutine inc1 (mgen1,dato1)

		implicit none

		class(mgenc1),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:)

		mgen1%n=mgen1%n+dato1

		return
			
	end subroutine inc1

	subroutine outc1 (mgen1,dato1)

		implicit none

		class(mgenc1),target,intent(in)				:: mgen1
		complex( kind = dp ),pointer,intent(out)		:: dato1(:)

		allocate(dato1(mgen1%ind(1)))
		dato1=>mgen1%n

		return
			
	end subroutine outc1

	subroutine  printc1(mgen1)

		implicit none

		class(mgenc1),intent(in)				:: mgen1

		print*, mgen1%n

		return

	end subroutine printc1

	function tipoc1 ()

		implicit none
		character ( len=20 )					:: tipoc1
		
		tipoc1 = 'mgenc1'

		return
			
	end function tipoc1
!********************************************************************************
!**************** TYPE MGENC2****************************************************
!********************************************************************************
	subroutine putc2 (mgen1,dato1,map)

		implicit none

		class(mgenc2),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1
		integer( kind = k9 ),intent(in)				:: map(2)

		mgen1%n(map(1),map(2))=mgen1%n(map(1),map(2))+dato1

		return
			
	end subroutine putc2

	subroutine putfc2 (mgen1,dato1,map)

		implicit none

		class(mgenc2),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:)
		integer( kind = k9 ),intent(in)				:: map(2)

		mgen1%n(map(1):map(1)+size(dato1)-1_k9,map(2))=mgen1%n(map(1):map(1)+size(dato1,1)-1_k9,map(2))+dato1

		return
			
	end subroutine putfc2

	subroutine putvc2 (mgen1,dato1,map)

		implicit none

		class(mgenc2),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:,:)
		integer( kind = k9 ),intent(in)				:: map(2)

		mgen1%n(map(1):map(1)+size(dato1,1)-1_k9,map(2):map(2)+size(dato1,2)-1_k9)=&
		mgen1%n(map(1):map(1)+size(dato1,1)-1_k9,map(2):map(2)+size(dato1,2)-1_k9)+dato1

		return
			
	end subroutine putvc2

	subroutine inc2 (mgen1,dato1)

		implicit none

		class(mgenc2),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:,:)

		mgen1%n=mgen1%n+dato1

		return
			
	end subroutine inc2

	subroutine outc2 (mgen1,dato1)

		implicit none

		class(mgenc2),target,intent(in)				:: mgen1
		complex( kind = dp ),pointer,intent(out)		:: dato1(:,:)

		call outc2in(mgen1%n,dato1)

		return
			
	end subroutine outc2

	subroutine outc2in (mgen1,dato1)

		implicit none

		complex( kind = dp ),target,intent(in)			:: mgen1(:,:)
		complex( kind = dp ),pointer,intent(out)		:: dato1(:,:)

		allocate(dato1(size(mgen1,1),size(mgen1,2)))
		dato1=>mgen1

		return
			
	end subroutine outc2in

	subroutine  printc2(mgen1)

		implicit none

		class(mgenc2),intent(in)				:: mgen1

		print*, mgen1%n

		return

	end subroutine printc2

	function tipoc2 ()

		implicit none
		character ( len=20 )					:: tipoc2
		
		tipoc2 = 'mgenc2'

		return
			
	end function tipoc2
!********************************************************************************
!**************** (DUMMY FUNCTIONS)**********************************************
!********************************************************************************
!********************************************************************************
!**************** TYPE MGEN******************************************************
!********************************************************************************
	subroutine put1 (mgen1,dato1,map)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1
		integer( kind = k9 ),intent(in)				:: map

		print*,'module kinds, subroutine put1'
		print*,'Dummy function'
		stop

		print*,dato1,map
		select type (mgen1)
			type is (mgen)
		end select

		print*,dato1,map

		return
			
	end subroutine put1

	subroutine put2 (mgen1,dato1,map)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1
		integer( kind = k9 ),intent(in)				:: map(2)

		print*,'module kinds, subroutine put1'
		print*,'Dummy function'
		stop

		print*,dato1,map
		select type (mgen1)
			type is (mgen)
		end select

		print*,dato1,map

		return
			
	end subroutine put2

	subroutine putv1 (mgen1,dato1,map)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:)
		integer( kind = k9 ),intent(in)				:: map

		print*,'module kinds, subroutine put1'
		print*,'Dummy function'
		stop

		print*,dato1
		select type (mgen1)
			type is (mgen)
		end select

		print*,dato1,map

		return
			
	end subroutine putv1

	subroutine putv2 (mgen1,dato1,map)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:,:)
		integer( kind = k9 ),intent(in)				:: map(2)

		print*,'module kinds, subroutine put2'
		print*,'Dummy function'
		stop

		print*,dato1
		select type (mgen1)
			type is (mgen)
		end select

		print*,dato1,map

		return
			
	end subroutine putv2

	subroutine in1 (mgen1,dato1)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:)

		print*,'module kinds, subroutine in1'
		print*,'Dummy function'
		stop

		print*,dato1
		select type (mgen1)
			type is (mgen)
		end select

		return
			
	end subroutine in1

	subroutine out1 (mgen1,dato1)

		implicit none

		class(mgen),target,intent(in)				:: mgen1
		real( kind = dp ),pointer,intent(out)			:: dato1(:)

		print*,'module algebras, subroutine out1'
		print*,'Dummy function'
		stop

		allocate (dato1(mgen1%ind(1)))
		select type (mgen1)
			type is (mgen)
		end select

		return
			
	end subroutine out1

	subroutine in2 (mgen1,dato1)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		real( kind = dp ),intent(in)				:: dato1(:,:)

		print*,'module kinds, subroutine in1'
		print*,'Dummy function'
		stop

		print*,dato1
		select type (mgen1)
			type is (mgen)
		end select

		return
			
	end subroutine in2

	subroutine out2 (mgen1,dato1)

		implicit none

		class(mgen),target,intent(in)				:: mgen1
		real( kind = dp ),pointer,intent(out)			:: dato1(:,:)

		print*,'module algebras, subroutine out2'
		print*,'Dummy function'
		stop

		allocate (dato1(mgen1%ind(1),mgen1%ind(2)))
		select type (mgen1)
			type is (mgen)
		end select

		return
			
	end subroutine out2

	subroutine print1(mgen1)

		implicit none

		class(mgen),intent(in)					:: mgen1

		print*,'module algebras, subroutine print1'
		print*,'Dummy function'
		stop

		select type (mgen1)
			type is (mgen)
		end select

		return

	end subroutine print1

	function tipo ()

		implicit none
		character ( len=20 )					:: tipo

		print*,'module algebras, subroutine tipo'
		print*,'Dummy function'
		stop
		
		tipo = 'mgen'

		return
			
	end function tipo

	subroutine put3 (mgen1,dato1,map)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1
		integer( kind = k9 ),intent(in)				:: map

		print*,'module kinds, subroutine put3'
		print*,'Dummy function'
		stop

		print*,dato1,map
		select type (mgen1)
			type is (mgen)
		end select

		print*,dato1,map

		return
			
	end subroutine put3

	subroutine put4 (mgen1,dato1,map)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1
		integer( kind = k9 ),intent(in)				:: map(2)

		print*,'module kinds, subroutine put4'
		print*,'Dummy function'
		stop

		print*,dato1,map
		select type (mgen1)
			type is (mgen)
		end select

		print*,dato1,map

		return
			
	end subroutine put4

	subroutine putv3 (mgen1,dato1,map)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:)
		integer( kind = k9 ),intent(in)				:: map

		print*,'module kinds, subroutine putv3'
		print*,'Dummy function'
		stop

		print*,dato1
		select type (mgen1)
			type is (mgen)
		end select

		print*,dato1,map

		return
			
	end subroutine putv3

	subroutine putv4 (mgen1,dato1,map)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:,:)
		integer( kind = k9 ),intent(in)				:: map(2)

		print*,'module kinds, subroutine putv4'
		print*,'Dummy function'
		stop

		print*,dato1
		select type (mgen1)
			type is (mgen)
		end select

		print*,dato1,map

		return
			
	end subroutine putv4

	subroutine in3 (mgen1,dato1)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:)

		print*,'module kinds, subroutine in3'
		print*,'Dummy function'
		stop

		print*,dato1
		select type (mgen1)
			type is (mgen)
		end select

		return
			
	end subroutine in3

	subroutine out3 (mgen1,dato1)

		implicit none

		class(mgen),target,intent(in)				:: mgen1
		complex( kind = dp ),pointer,intent(out)		:: dato1(:)

		print*,'module algebras, subroutine out3'
		print*,'Dummy function'
		stop

		allocate (dato1(mgen1%ind(1)))
		select type (mgen1)
			type is (mgen)
		end select

		return
			
	end subroutine out3

	subroutine in4 (mgen1,dato1)

		implicit none

		class(mgen),intent(inout)				:: mgen1
		complex( kind = dp ),intent(in)				:: dato1(:,:)

		print*,'module kinds, subroutine in4'
		print*,'Dummy function'
		stop

		print*,dato1
		select type (mgen1)
			type is (mgen)
		end select

		return
			
	end subroutine in4

	subroutine out4 (mgen1,dato1)

		implicit none

		class(mgen),target,intent(in)				:: mgen1
		complex( kind = dp ),pointer,intent(out)		:: dato1(:,:)

		print*,'module algebras, subroutine out4'
		print*,'Dummy function'
		stop

		allocate (dato1(mgen1%ind(1),mgen1%ind(2)))
		select type (mgen1)
			type is (mgen)
		end select

		return
			
	end subroutine out4

end Module algebras