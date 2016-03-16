Module puntos

!********************************************************************************
!***LAST MODIFICATION: 12/08/09 (DDMMYY)
!***DESCRIPTION: Object type point (coordinates) and his associated procedures (constructor,read data)
!***USE:	Module Kinds for common type definitions.
	
!	CONTAINS:
!		- Public : Type definition 'punto'. Includes
!			DATA
!			- Private : coor(:) - allocatable real array containing coordinates (1D-ND)
!			PROCEDURES
!			- new: creates a 'punto' object instance
!		        	- Interfaz
!					- IN - coor1(:) - real array containing coordinates
!					- OUT  - punto1 - type 'punto' instance
!			- dimen (function): gets 'punto' dimension
!		        	- Interfaz
!					- OUT  - dimen - integer: dimension of the coordinates contained
!			- out: returns a pointer referenced to the array containing coordinates
!		        	- Interfaz
!					- OUT  - coor1(:) - real pointer to coordinates		 		    
!		- Public : Type definition 'ppunto'. Includes
!			DATA
!			- Public : p - type 'punto' pointer
!		- Public : Type definition 'markpunt'. It has been defined to provide one self-contained type with
!				information about the nodes of an element. It will be used to avoid duplication of nodes.Includes
!			DATA
!			- Public :  marca(:) - integer allocatable array with information about nodes
!			- Public :  vpuntos(:) - type 'punto' allocatable array
!********************************************************************************

use kinds

implicit none

private

public punto,ppunto,mpunto,marcpunt,puntnormal
!********************************************************************************
!**************** DECLARACIONES *************************************************
!********************************************************************************
	type				:: punto
		real( kind = dp ),allocatable,private	:: coor(:)
	contains
		procedure				:: new => newpunto
		procedure				:: dimen=>dimpunto
		procedure				:: out => leecoordenadaspunto
		procedure				:: id => idpunto
	end type punto
!********************************************************************************
	type				:: ppunto
		type( punto ),pointer			:: p
	end type ppunto
!********************************************************************************
	type				:: mpunto
		type( punto ),allocatable		:: n(:)
	end type mpunto
!********************************************************************************
	type 				:: marcpunt
		integer(kind=k9),allocatable		:: marca(:)
		type(punto),allocatable			:: vpuntos(:)
	end type marcpunt
!********************************************************************************
	type 				:: puntnormal
		type(punto)				:: p
		real(kind=dp),allocatable		:: normal(:)
	end type puntnormal
!********************************************************************************
contains
!********************************************************************************
!**************** TYPE PUNTO*****************************************************
!********************************************************************************
	subroutine newpunto (punto1,coor1)

		implicit none

		class(punto),intent(out)				:: punto1
		real( kind = dp ),intent(in)				:: coor1(:)

		allocate (punto1%coor(size(coor1)))

		punto1%coor=coor1

		return
			
	end subroutine newpunto

	function dimpunto (punto1)

		implicit none

		class(punto),intent(in)					:: punto1
		integer ( kind = k9 )					:: dimpunto

		dimpunto=size(punto1%coor)

		return
			
	end function dimpunto

	subroutine leecoordenadaspunto (punto1,coor1)

		implicit none

		class(punto),intent(in),target				:: punto1
		real( kind = dp ),pointer,intent(out)			:: coor1(:)

		coor1=>punto1%coor

		return
			
	end subroutine leecoordenadaspunto

	function idpunto (punto1,punto2)

		implicit none

		class(punto),intent(in)					:: punto1,punto2
		logical							:: idpunto
		real( kind = dp ),pointer				:: coor1(:),coor2(:)
		real( kind = dp )					:: temp,epsilon1=0.000001_dp
		integer( kind = k9 )					:: i

		call punto1%out(coor1)
		call punto2%out(coor2)

		temp=0.0_dp

		do i=1,size(coor1)
			temp = temp + abs (coor1(i)-coor2(i))
		end do

		if (temp.LT.epsilon1) then
			idpunto=.TRUE.
		else
			idpunto=.FALSE.
		end if

		return
			
	end function idpunto
	
End Module puntos