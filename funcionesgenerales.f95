module funcionesgenerales

!********************************************************************************
!***LAST MODIFICATION: 18/11/09 (DDMMYY)
!***DESCRIPTION: General math functions
!***USE:	Module Kinds for common type definitions.
!	CONTAINS:
!		- Public : function kronecker: delta kronecker function
!		        - Interfaz
!				- IN - real - a,b - real scalar
!				- OUT - real - kronecker - real scalar (1 or 0)

!		- Public : function dot_vect: vectorial dot of two vectors
!		        - Interfaz
!				- IN - real - a(3),b(3) - real arrays (vectors)
!				- OUT  -real -  dot_vect(3) - real array

!		- Public : function mpointer2matrix: transform mpointer into real matrix
!		        - Interfaz
!				- IN - type mpointer - a(:) - pointer with data
!				- OUT  - real - mpointer2matrix(:,:) - allocatable real matrix 

!		- Public : function mpointer2vector: transform mpointer into real array
!		        - Interfaz
!				- IN - type mpointer - a(:) - pointer with data
!				- OUT  - real - mpointer2vector(:) - allocatable real array
!********************************************************************************
use kinds
use algebras

implicit none

private

public kronecker,dot_vect,mpointer2matrix,mpointer2vector,LU,gfunc,mgfunc,inv2o3,Mnorm,erf,vectorsum,Dsyevj3

!********************************************************************************
!**************** DECLARATIONS **************************************************
!********************************************************************************
	type gfunc
		character(len=2)			:: s
		real( kind = dp )			:: r1
		character(len=3)			:: a
		integer( kind = k9 )			:: i1
		real( kind = dp )			:: r2
		integer( kind = k9 ),allocatable	:: i2(:)
	contains
 		procedure				:: eval => generical1
	end type gfunc
!********************************************************************************
	type mgfunc
		type(gfunc),allocatable			:: n(:)
	contains
 		procedure				:: eval => generical
	end type mgfunc
!********************************************************************************
	interface dot_vect
		  module procedure dot_vect,dot_vect_c
	end interface

	interface mpointer2matrix
		  module procedure mpointer2matrix,mpointer2matrix_c
	end interface

	interface mpointer2vector
		  module procedure mpointer2vector,mpointer2vector_c
	end interface

	interface inv2o3
		  module procedure inv2o3,inv2o3_c
	end interface
!********************************************************************************
contains
!********************************************************************************
!**************** FUNCIONES GLOBALES*********************************************
!********************************************************************************
	function kronecker (a,b)
	
		implicit none
	
		integer( kind = k9 ),intent(in)				:: a,b
		real( kind = dp )					:: kronecker
	
		if (a.eq.b) then
			kronecker = 1.0_dp
		else
			kronecker = 0.0_dp
		end if
	
		return
			
	end function kronecker

	function vectorsum (vector)
	
		implicit none
	
		real( kind = dp ),intent(in)				:: vector(:)
		real( kind = dp )					:: vectorsum
		integer( kind = k9 )					:: i

		vectorsum = 0.0_dp
	
		do i=1,size(vector)
			vectorsum = vectorsum + abs(vector(i))
		end do
	
		return
			
	end function vectorsum

	function erf (x)
	
		implicit none
	
		real( kind = dp ),intent(in)				:: x
		real( kind = dp )					:: erf,t

		t = 1.0_dp/(1.0_dp + 0.3275911_dp*x)

		erf = 0.254829592_dp*t - 0.284496736_dp*(t**2) + 1.421413741_dp*(t**3) - 1.453152027_dp*(t**4) + 1.061405429_dp*(t**5)

		erf = 1.0_dp - erf*exp(-(x**2))
	
		return
			
	end function erf
	
	function dot_vect (a,b)
	
		implicit none
	
		real( kind = dp ),intent(in)				:: a(3),b(3)
		real( kind = dp )					:: dot_vect(3)
		
		dot_vect(1) = a(2)*b(3) - a(3)*b(2)
		dot_vect(2) = a(3)*b(1) - a(1)*b(3)
		dot_vect(3) = a(1)*b(2) - a(2)*b(1)
	
		return
			
	end function dot_vect

	function dot_vect_c (a,b)
	
		implicit none
	
		complex( kind = dp ),intent(in)				:: a(3),b(3)
		complex( kind = dp )					:: dot_vect_c(3)
		
		dot_vect_c(1) = a(2)*b(3) - a(3)*b(2)
		dot_vect_c(2) = a(3)*b(1) - a(1)*b(3)
		dot_vect_c(3) = a(1)*b(2) - a(2)*b(1)
	
		return
			
	end function dot_vect_c

	function mpointer2matrix (a)
	
		implicit none
	
		type(mpointer),intent(in)				:: a(:)
		real( kind = dp ),allocatable				:: mpointer2matrix(:,:)

		integer( kind = k9 )					:: i1,i2,i,j		

		i1=size(a(1)%n)
		i2=size(a)

		allocate (mpointer2matrix(i1,i2))

		do i=1,i1
			do j=1,i2
				 mpointer2matrix(i,j) = a(j)%n(i) 
			end do
		end do

	
		return
			
	end function mpointer2matrix

	function mpointer2matrix_c (a)
	
		implicit none
	
		type(mpointer_c),intent(in)				:: a(:)
		complex( kind = dp ),allocatable			:: mpointer2matrix_c(:,:)

		integer( kind = k9 )					:: i1,i2,i,j		

		i1=size(a(1)%n)
		i2=size(a)

		allocate (mpointer2matrix_c(i1,i2))

		do i=1,i1
			do j=1,i2
				 mpointer2matrix_c(i,j) = a(j)%n(i) 
			end do
		end do

		return
			
	end function mpointer2matrix_c

	function mpointer2vector (a)
	
		implicit none
	
		type(mpointer),intent(in)				:: a(:)
		real( kind = dp ),allocatable				:: mpointer2vector(:)

		integer( kind = k9 )					:: i1,i2,i,j		

		i1=size(a(1)%n)
		i2=size(a)

		allocate (mpointer2vector(i1*i2))

		do i=1,i1
			do j=1,i2
				 mpointer2vector((j-1)*i1 + i) = a(j)%n(i) 
			end do
		end do

	
		return
			
	end function mpointer2vector

	function mpointer2vector_c (a)
	
		implicit none
	
		type(mpointer_c),intent(in)				:: a(:)
		complex( kind = dp ),allocatable			:: mpointer2vector_c(:)

		integer( kind = k9 )					:: i1,i2,i,j		

		i1=size(a(1)%n)
		i2=size(a)

		allocate (mpointer2vector_c(i1*i2))

		do i=1,i1
			do j=1,i2
				 mpointer2vector_c((j-1)*i1 + i) = a(j)%n(i) 
			end do
		end do

	
		return
			
	end function mpointer2vector_c
!********************************************************************************
!****************INVERSOR********************************************************
!********************************************************************************
	subroutine inv2o3(A)

		implicit none

		real( kind = dp ),intent(inout)				:: A(:,:)
		real( kind = dp ),allocatable 				:: TA(:,:)
		real( kind = dp )					:: det
		integer(kind=k9)					:: s

		s=size(A,1)
				
		allocate (TA(s,s))

		select case (s)

			case(2)

			case(3)		
				TA(1,1)=A(2,2)*A(3,3) - A(2,3)*A(3,2)
				TA(1,2)=A(2,3)*A(3,1) - A(2,1)*A(3,3) 
				TA(1,3)=A(2,1)*A(3,2) - A(2,2)*A(3,1)
				TA(2,1)=A(1,3)*A(3,2) - A(1,2)*A(3,3) 
				TA(2,2)=A(1,1)*A(3,3) - A(1,3)*A(3,1)
				TA(2,3)=A(1,2)*A(3,1) - A(1,1)*A(3,2)
				TA(3,1)=A(1,2)*A(2,3) - A(1,3)*A(2,2)	  
				TA(3,2)=A(1,3)*A(2,1) - A(1,1)*A(2,3) 
				TA(3,3)=A(1,1)*A(2,2) - A(1,2)*A(2,1)
			      
				TA = transpose(TA)

				det = A(1,1)*A(2,2)*A(3,3) + A(1,2)*A(2,3)*A(3,1) + A(1,3)*A(2,1)*A(3,2)

				det = det - A(1,1)*A(2,3)*A(3,2) - A(1,2)*A(2,1)*A(3,3) - A(1,3)*A(2,2)*A(3,1)

				TA = TA/det
					
			case default

		end select

		A = TA

		return
  
	end subroutine inv2o3

	subroutine inv2o3_c(A)

		implicit none

		complex( kind = dp ),intent(inout)			:: A(:,:)
		complex( kind = dp ),allocatable 			:: TA(:,:)
		complex( kind = dp )					:: det
		integer(kind=k9)					:: s

		s=size(A,1)
				
		allocate (TA(s,s))

		select case (s)

			case(2)

			case(3)		
				TA(1,1)=A(2,2)*A(3,3) - A(2,3)*A(3,2)
				TA(1,2)=A(2,3)*A(3,1) - A(2,1)*A(3,3) 
				TA(1,3)=A(2,1)*A(3,2) - A(2,2)*A(3,1)
				TA(2,1)=A(1,3)*A(3,2) - A(1,2)*A(3,3) 
				TA(2,2)=A(1,1)*A(3,3) - A(1,3)*A(3,1)
				TA(2,3)=A(1,2)*A(3,1) - A(1,1)*A(3,2)
				TA(3,1)=A(1,2)*A(2,3) - A(1,3)*A(2,2)	  
				TA(3,2)=A(1,3)*A(2,1) - A(1,1)*A(2,3) 
				TA(3,3)=A(1,1)*A(2,2) - A(1,2)*A(2,1)
			      
				TA = transpose(TA)

				det = A(1,1)*A(2,2)*A(3,3) + A(1,2)*A(2,3)*A(3,1) + A(1,3)*A(2,1)*A(3,2)

				det = det - A(1,1)*A(2,3)*A(3,2) - A(1,2)*A(2,1)*A(3,3) - A(1,3)*A(2,2)*A(3,1)

				TA = TA/det
					
			case default

		end select

		A = TA

		return
  
	end subroutine inv2o3_c

!********************************************************************************
!****************INVERSOR LU RETOCADO********************************************
!********************************************************************************

 	subroutine LU(A,B)

		implicit none

		class(mgen),target,intent(in)					:: A,B

		real( kind = dp ),pointer					:: ma(:,:),mb(:)
		integer(kind=k9),allocatable					:: idx(:)
		real(kind=dp),allocatable					:: vscal(:)
		real(kind=dp)							:: cn,condn

		select case (A%tipo())
			case ('mgenr2')

				call A%out(ma)
				call B%out(mb)

				call prescal(ma,mb)
				print*,'prescal'
				call scal(ma,vscal)
				print*,'scal'
				call Mnorm(ma,cn)
				condn = cn
				call LUdcmp(ma,idx)
				print*,'LUdcmp'
				call LUbksb(ma,idx,mb)
				call Mnorm(ma,cn)
				condn = condn*cn
				print*,'LUbksb'
				call scalout(mb,vscal)
				print*,'scalout'
				print*,'condition number',condn
			case default
				print*,'module funcionesgenerales, subroutine LU'
				print*,'Tipo de dato aun no implementado'
				stop
		end select
             
		return
 
 	end subroutine LU

	subroutine prescal(A,B)

		implicit none
		
		real(kind=dp),pointer, intent(inout)				:: A(:,:),B(:)

		integer(kind=k9)						:: n,i,j
		real(kind=dp)							:: big
	      
		n=size(A,1_k9)

 		do i=1,n
			big = 0.0_dp
			do j=1,n
				if(abs(A(i,j)).gt.abs(big)) big = A(i,j)		
			end do

			A(i,:)= A(i,:)/big
			B(i)  = B(i)/big
 		end do

		return
	
	end subroutine prescal

	subroutine scal(A,vscal)

		implicit none
		
		real(kind=dp),pointer, intent(inout)				:: A(:,:)
		real(kind=dp),allocatable,intent(out)				:: vscal(:)

		integer(kind=k9)						:: n,i,j
		real(kind=dp)							:: big
	      
		n=size(A,1_k9)
 		allocate(vscal(n))
		vscal = 0.0_dp

 		do i=1,n
			big = 0.0_dp
			do j=1,n
				if(abs(A(j,i)).gt.abs(big)) big = A(j,i)			
			end do
	
			vscal(i)=big
			A(:,i)=A(:,i)/vscal(i)
 		end do

		return
	
	end subroutine scal

	subroutine Mnorm(A,cn)

		implicit none 

		real(kind=dp),pointer,intent(in)				:: A(:,:)
		real(kind=dp),intent(out)					:: cn

		integer(kind=k9)						:: i,j

		cn = 0.0_dp

		do i=1,size(A,1)

			do j=1,size(A,1)

				cn = cn + a(i,j)**2.0_dp

			end do

		end do

		cn = sqrt(cn)

	end subroutine Mnorm

	subroutine LUdcmp(A,idx)

		implicit none
		
		real(kind=dp),pointer,intent(inout)				:: A(:,:)
		integer(kind=k9),allocatable,intent(out)			:: idx(:)

		real(kind=dp),allocatable					:: VV(:)
		integer(kind=k9)						:: i,imax,j,k,d,n
		real(kind=dp)							:: big,dum,suma,temp
		real(kind=dp)							:: tiny = 1.E-20
	      
		n=size(A,1_k9)
		allocate(VV(n))
		allocate(idx(n))

		d = 1_k9

		do i=1,n
			big = 0.0_dp
			do j=1,n
				temp = A(i,j)
				if(abs(temp).gt.abs(big)) big = temp
			end do
			if(abs(big).eq.0.0_dp) stop ' Matriz Singular en LUdcmp'
			VV(i)=1.0_dp/big
		end do
	      
		do j=1,n
			do i=1,j-1
				suma = A(i,j)
				do k=1,i-1
					suma = suma - A(i,k)*A(k,j)
				end do
				A(i,j) = suma
			end do
			big = 0.0_dp
			do i=j,n
				suma = A(i,j)
				do k=1,j-1
					suma = suma - A(i,k)*A(k,j)
				end do
				A(i,j) = suma
				dum = VV(i)*suma
				if(abs(dum).GE.abs(big)) then
					big = dum
					imax = i
				end if
			end do
			if(j.NE.imax) THEN
				do k=1,n
					dum = A(imax,k)
					A(imax,k) = A(j,k)
					A(j,k) = dum
				end do
				d = -d
				VV(imax) = VV(j)
			end if
			idx(j) = imax  
			if(A(j,j).EQ.0.0_dp) then
				A(j,j) = TINY
				print*, ' pivote en j= ',j, ' es tiny'
			end if
			if(j.NE.n) then
				dum = 1.0_dp/A(j,j)
				do i=j+1,n
					A(i,j) = A(i,j)*dum
				end do
			end if
		end do

		return
	
	end subroutine LUdcmp

 	subroutine LUbksb(A,idx,B)

		implicit none
		
		real(kind=dp),pointer,intent(in)				:: A(:,:)
		integer(kind=k9),intent(in)					:: idx(:)
		real(kind=dp),pointer,intent(inout)				:: B(:)

		integer(kind=k9)						:: i,ii,ip,j,n
		real(kind=dp)							:: suma
	      
		ii=0_k9
		n=size(A,1_k9)
		do i=1,n
			ip = idx(i)
			suma = B(ip)
			B(ip) = B(i)
			if(ii.ne.0) then
				do j=ii,i-1
					suma = suma - A(i,j)*B(j)
				end do
			elseif(suma.ne.0.) then
				ii = i
			end if
			B(i) = suma
		end do
	      
		do i = N,1,-1
			suma = B(i)
			do j=i+1,n
				suma = suma - A(i,j)*B(j)
			end do
			B(i) = suma/a(i,i)
		end do         
 
		return

 	end subroutine LUbksb

	subroutine scalout(X,vscal)

		implicit none
		
		real(kind=dp),pointer,intent(inout)				:: X(:)
		real(kind=dp),intent(in)					:: vscal(:)

		integer(kind=k9)						:: i

 		do i=1,size(X)
			X(i)=X(i)/vscal(i)
 		end do

		return
	
	end subroutine scalout
!********************************************************************************
!****************TYPE MGFUNC*****************************************************
!********************************************************************************
	function generical(dato,base,base2)

		implicit none
		
		real(kind=dp)							:: generical
		class (mgfunc),intent(in)					:: dato
		real(kind=dp),optional,intent(in)				:: base(:)
		real(kind=dp),optional,intent(in)				:: base2(:)
		real(kind=dp)							:: generical2
		real(kind=dp)							:: generical3
		real(kind=dp)							:: generical4
		real(kind=dp),allocatable					:: base1(:)
		integer(kind=k9)						:: i

		generical=0.0_dp
		generical2=0.0_dp
		generical3=0.0_dp
		generical4=0.0_dp

		if ((present(base)).and.(present(base2))) then
			allocate(base1(size(base)+size(base2)))
			base1(1:size(base))=base
			base1(size(base)+1:size(base)+size(base2))=base2
		elseif (present(base)) then
			allocate(base1(size(base)))
			base1=base
		elseif (present(base2)) then
			allocate(base1(size(base2)))
			base1=base2
		else
			base1(size(base)+1:size(base)+size(base2))=base2
			allocate(base1(1_k9))	
			base1=1_k9
		end if

		do i=1,size(dato%n)
			select case(dato%n(i)%s)
				case('+')
					generical = generical + generical1(dato%n(i),base1)
				case('*')
					generical = generical * generical1(dato%n(i),base1)
				case('-')
					generical = generical - generical1(dato%n(i),base1)
				case('/')
					generical = generical / generical1(dato%n(i),base1)
				case('^')
					generical = generical**nint(generical1(dato%n(i),base1))
				case('s')
					generical = sqrt(generical)
				case('(+')
					generical2 = generical2 + generical1(dato%n(i),base1)
				case('(*')
					generical2 = generical2 * generical1(dato%n(i),base1)
				case('(-')
					generical2 = generical2 - generical1(dato%n(i),base1)
				case('(d')
					generical2 = generical2 / generical1(dato%n(i),base1)
				case('(^')
					generical2 = generical2**nint(generical1(dato%n(i),base1))
				case('(s')
					generical2 = sqrt(generical2)
				case('(c')
					generical2 = cos(generical2)
				case('(z')
					generical2 = sin(generical2)
				case('[+')
					generical3 = generical3 + generical1(dato%n(i),base1)
				case('[*')
					generical3 = generical3 * generical1(dato%n(i),base1)
				case('[-')
					generical3 = generical3 - generical1(dato%n(i),base1)
				case('[d')
					generical3 = generical3 / generical1(dato%n(i),base1)
				case('[^')
					generical3 = generical3**nint(generical1(dato%n(i),base1))
				case('[s')
					generical3 = sqrt(generical3)
				case('{+')
					generical4 = generical4 + generical1(dato%n(i),base1)
				case('{*')
					generical4 = generical4 * generical1(dato%n(i),base1)
				case('{-')
					generical4 = generical4 - generical1(dato%n(i),base1)
				case('{d')
					generical4 = generical4 / generical1(dato%n(i),base1)
				case('{^')
					generical4 = generical4**nint(generical1(dato%n(i),base1))
				case('{s')
					generical4 = sqrt(generical4)
				case(')+')
					generical = generical + generical2
					generical2=0.0_dp
				case(')*')
					generical = generical * generical2
					generical2=0.0_dp
				case(')-')
					generical = generical - generical2
					generical2=0.0_dp
				case(')d')
					generical = generical / generical2
					generical2=0.0_dp
				case(')e')
					generical = generical2 / generical
					generical2=0.0_dp
				case(')^')
					generical = generical**nint(generical2)
					generical2=0.0_dp
				case(']+')
					generical2 = generical2 + generical3
					generical3=0.0_dp
				case(']*')
					generical2 = generical2 * generical3
					generical3=0.0_dp
				case(']-')
					generical2 = generical2 - generical3
					generical3=0.0_dp
				case(']d')
					generical2 = generical2 / generical3
					generical3=0.0_dp
				case(']e')
					generical2 = generical3 / generical2
					generical3=0.0_dp
				case(']^')
					generical2 = generical2**nint(generical3)
					generical3=0.0_dp
				case('}+')
					generical3 = generical3 + generical4
					generical4=0.0_dp
				case('}*')
					generical3 = generical3 * generical4
					generical4=0.0_dp
				case('}-')
					generical3 = generical3 - generical4
				case('}d')
					generical3 = generical3 / generical4
					generical4=0.0_dp
				case('}e')
					generical3 = generical4 / generical3
					generical4=0.0_dp
				case('}^')
					generical3 = generical3**nint(generical4)
					generical4=0.0_dp
				case default
					print*,dato%n(i)%s
					print*,'module funcionesgenerales, subroutine generic' 
					print*,'Función desconocida'
					stop 	
			end select
		end do

		return
	
	end function generical
!********************************************************************************
!****************TYPE GFUNC******************************************************
!********************************************************************************
	function generical1 (dato,base)

		implicit none
		
		real(kind=dp)							:: generical1
		class (gfunc),intent(in)					:: dato
		real(kind=dp),intent(in)					:: base(:)
		integer(kind=k9)						:: i

		generical1=0.0_dp

		select case (dato%a)
			case('con')
				generical1=dato%r1
			case('pol')
				generical1=generical1 + base(dato%i2(1))**(dato%i2(2))
				do i=2,size(dato%i2)/2_k9
					generical1=generical1*base(dato%i2(2*i-1))**(dato%i2(2*i))
				end do
				generical1=dato%r1*(dato%r2*generical1)**dato%i1
			case('log')
				generical1=generical1 + base(dato%i2(1))**(dato%i2(2))
				do i=2,size(dato%i2)/2_k9
					generical1=generical1*base(dato%i2(2*i-1))**(dato%i2(2*i))
				end do
				generical1=dato%r1*(log(dato%r2*generical1))**dato%i1
			case('exp')
				generical1=generical1 + base(dato%i2(1))**(dato%i2(2))
				do i=2,size(dato%i2)/2_k9
					generical1=generical1*base(dato%i2(2*i-1))**(dato%i2(2*i))
				end do
				generical1=dato%r1*(exp(dato%r2*generical1))**dato%i1
			case('sin')
				generical1=generical1 + base(dato%i2(1))**(dato%i2(2))
				do i=2,size(dato%i2)/2_k9
					generical1=generical1*base(dato%i2(2*i-1))**(dato%i2(2*i))
				end do
				generical1=dato%r1*(sin(dato%r2*generical1))**dato%i1
			case('cos')
				generical1=generical1 + base(dato%i2(1))**(dato%i2(2))
				do i=2,size(dato%i2)/2_k9
					generical1=generical1*base(dato%i2(2*i-1))**(dato%i2(2*i))
				end do
				generical1=dato%r1*(cos(dato%r2*generical1))**dato%i1
			case('tan')
				generical1=generical1 + base(dato%i2(1))**(dato%i2(2))
				do i=2,size(dato%i2)/2_k9
					generical1=generical1*base(dato%i2(2*i-1))**(dato%i2(2*i))
				end do
				generical1=dato%r1*(tan(dato%r2*generical1))**dato%i1
			case default
				print*,'module funcionesgenerales, subroutine generical' 
				print*,'Función no implementada'
				stop 
		end select

		return
	
	end function generical1

! * ----------------------------------------------------------------------------
! * Numerical diagonalization of 3x3 matrcies
! * Copyright (C) 2006  Joachim Kopp
! * ----------------------------------------------------------------------------
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation; either
! * version 2.1 of the License, or (at your option) any later version.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the Free Software
! * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
! * ----------------------------------------------------------------------------


! * ----------------------------------------------------------------------------
      SUBROUTINE Dsyevj3 (A, Q, W)
! * ----------------------------------------------------------------------------
! * Calculates the eigenvalues and normalized eigenvectors of a symmetric 3x3
! * matrix A using the Jacobi algorithm.
! * The upper triangular part of A is destroyed during the calculation,
! * the diagonal elements are read but not destroyed, and the lower
! * triangular elements are not referenced at all.
! * ----------------------------------------------------------------------------
! * Parameters:
! *   A: The symmetric input matrix
! *   Q: Storage buffer for eigenvectors
! *   W: Storage buffer for eigenvalues
! * ----------------------------------------------------------------------------
! *     .. Arguments ..
      DOUBLE PRECISION A(3,3)
      DOUBLE PRECISION Q(3,3)
      DOUBLE PRECISION W(3)

! *     .. Parameters ..
      INTEGER          N
      PARAMETER        ( N = 3 )
    
! *     .. Local Variables ..
      DOUBLE PRECISION SD, SO
      DOUBLE PRECISION S, C, T
      DOUBLE PRECISION G, H, Z, THETA
      DOUBLE PRECISION THRESH
      INTEGER          I, X, Y, R

! *     Initialize Q to the identitity matrix
! *     --- This loop can be omitted if only the eigenvalues are desired ---
      DO 10 X = 1, N
        Q(X,X) = 1.0D0
        DO 11, Y = 1, X-1
          Q(X, Y) = 0.0D0
          Q(Y, X) = 0.0D0
   11   CONTINUE
   10 CONTINUE

! *     Initialize W to diag(A)
      DO 20 X = 1, N
        W(X) = A(X, X)
   20 CONTINUE

! *     Calculate SQR(tr(A))  
      SD = 0.0D0
      DO 30 X = 1, N
        SD = SD + ABS(W(X))
   30 CONTINUE
      SD = SD**2
 
! *     Main iteration loop
      DO 40 I = 1, 50
! *       Test for convergence
        SO = 0.0D0
        DO 50 X = 1, N
          DO 51 Y = X+1, N
            SO = SO + ABS(A(X, Y))
   51     CONTINUE
   50   CONTINUE
        IF (SO .EQ. 0.0D0) THEN
          RETURN
        END IF

        IF (I .LT. 4) THEN
          THRESH = 0.2D0 * SO / N**2
        ELSE
          THRESH = 0.0D0
        END IF

! *       Do sweep
        DO 60 X = 1, N
          DO 61 Y = X+1, N
            G = 100.0D0 * ( ABS(A(X, Y)) )
            IF ( I .GT. 4 .AND. ABS(W(X)) + G .EQ. ABS(W(X)).AND. ABS(W(Y)) + G .EQ. ABS(W(Y)) ) THEN
              A(X, Y) = 0.0D0
            ELSE IF (ABS(A(X, Y)) .GT. THRESH) THEN
! *             Calculate Jacobi transformation
              H = W(Y) - W(X)
              IF ( ABS(H) + G .EQ. ABS(H) ) THEN
                T = A(X, Y) / H
              ELSE
                THETA = 0.5D0 * H / A(X, Y)
                IF (THETA .LT. 0.0D0) THEN
                  T = -1.0D0 / (SQRT(1.0D0 + THETA**2) - THETA)
                ELSE
                  T = 1.0D0 / (SQRT(1.0D0 + THETA**2) + THETA)
                END IF
              END IF

              C = 1.0D0 / SQRT( 1.0D0 + T**2 )
              S = T * C
              Z = T * A(X, Y)
              
! *             Apply Jacobi transformation
              A(X, Y) = 0.0D0
              W(X)    = W(X) - Z
              W(Y)    = W(Y) + Z
              DO 70 R = 1, X-1
                T       = A(R, X)
                A(R, X) = C * T - S * A(R, Y)
                A(R, Y) = S * T + C * A(R, Y)
   70         CONTINUE
              DO 80, R = X+1, Y-1
                T       = A(X, R)
                A(X, R) = C * T - S * A(R, Y)
                A(R, Y) = S * T + C * A(R, Y)
   80         CONTINUE
              DO 90, R = Y+1, N
                T       = A(X, R)
                A(X, R) = C * T - S * A(Y, R)
                A(Y, R) = S * T + C * A(Y, R)
   90         CONTINUE

! *             Update eigenvectors
! *             --- This loop can be omitted if only the eigenvalues are desired ---
              DO 100, R = 1, N
                T       = Q(R, X)
                Q(R, X) = C * T - S * Q(R, Y)
                Q(R, Y) = S * T + C * Q(R, Y)
  100         CONTINUE
            END IF
   61     CONTINUE
   60   CONTINUE
   40 CONTINUE

      PRINT *, "DSYEVJ3: No convergence."
            
      END SUBROUTINE Dsyevj3
! * End of subroutine DSYEVJ3

 end module funcionesgenerales
