Module kinds

!********************************************************************************
!***LAST MODIFICATION: 13/11/09 (DDMMYY)
!***DESCRIPTION: This module includes simply general types shared for most of the program.
!	CONTAINS:
!		- Public : k9,dp,sp: Common type definitions for real and integer types

!		- Public : Type 'rpointer'.Used  to get an array of real pointers. Includes
!			DATA
!			- Public: p - real pointer. 

!		- Public : Type 'mpointer'.Used  to get a matrix of real pointers. Really the dimension of every 
!					   row can be different.
!			DATA
!			- Public: n(:) - array of real pointers. 

!		- Public : Type 'minteger'.Used  to get an array of arrays of integers. Really is a "matrix" with 
!					   rows of different lenght
!			DATA
!			- Public: n(:) - allocatable array of integers. 

!		- Public : Type 'mreal1'.Used  to get an array of arrays of reals. Really is a "matrix" with 
!					   rows of different lenght.
!			DATA
!			- Public: n(:) - allocatable array of reals. 

!		- Public : Type 'mreal2'.Used  to get an array of matrix of reals. Really is an array with elements that they're
!					   matrix of different lenghts
!			DATA
!			- Public: n(:,:) - allocatable array of reals. 

!		- Public : Type 'mcharac'.Used  to get an array of characs with dim. It will be used for arrays of labels
!			DATA
!			- Public: n - caharacter with dim=20
!********************************************************************************
implicit none

private

public k9,dp,sp,qp,mpointer,m2pointer,rpointer,minteger,vminteger,minteger2,mcharac,mreal1,mreal1pointer
public mreal2,mreal2pointer,vmreal1,container,pcontainer,container2,pcontainer2,container3,container4,container5
public mpointer_c
!********************************************************************************
!**************** DECLARATIONS **************************************************
!********************************************************************************
! 	integer, parameter :: k9=selected_int_kind(9), dp=selected_real_kind(15,307), sp=selected_real_kind(6,37)
	integer, parameter :: k9=selected_int_kind(9), dp=selected_real_kind(15,307), sp=selected_real_kind(6,37)	
	integer, parameter :: qp = selected_real_kind(2*precision(1.0_dp))		
!********************************************************************************
	type rpointer
		real( kind = dp ),pointer		:: n => null()
	end type rpointer
!********************************************************************************
	type mpointer
		real( kind = dp ),pointer		:: n(:) => null()
	end type mpointer
!********************************************************************************
	type mpointer_c
		complex( kind = dp ),pointer		:: n(:) => null()
	end type mpointer_c
!********************************************************************************
	type m2pointer
		real( kind = dp ),pointer		:: n(:,:) => null()
	end type m2pointer
!********************************************************************************
	type minteger
		integer( kind = k9 ),allocatable	:: n(:)
	end type minteger
!********************************************************************************
	type vminteger
		type(minteger),allocatable		:: n(:)
	end type vminteger
!********************************************************************************
	type minteger2
		integer( kind = k9 ),allocatable	:: n(:,:)
	end type minteger2
!********************************************************************************
	type mreal1
		real( kind = dp ),allocatable		:: n(:)
	end type mreal1
!********************************************************************************
	type mreal1pointer
		type( mreal1 ),pointer			:: n => null()
	end type mreal1pointer
!********************************************************************************
	type vmreal1
		type (mreal1),allocatable		:: n(:)
	end type vmreal1
!********************************************************************************
	type mreal2
		real( kind = dp ),allocatable		:: n(:,:)
	end type mreal2
!********************************************************************************
	type mreal2pointer
		type( mreal2 ),pointer			:: n => null()
	end type mreal2pointer
!********************************************************************************
	type mcharac
		character(len=20)			:: n
	end type mcharac
!********************************************************************************
	type gfunc
		character(len=1)			:: s
		real( kind = dp )			:: r1
		character(len=3)			:: a
		integer( kind = k9 )			:: i1
		real( kind = dp )			:: r2
		integer( kind = k9 ),allocatable	:: i2(:)
	end type gfunc
!********************************************************************************
	type mgfunc
		type(gfunc),allocatable			:: n(:)
	end type mgfunc
!********************************************************************************
	type container
		integer( kind = k9 )			:: ind1
		real( kind = dp ),allocatable		:: base(:,:),jacobiano(:,:)
		type(vmreal1 ),allocatable		:: normal(:),coorreal(:)
		type(mreal2 ),allocatable		:: matrizphi(:,:)
	end type container
!********************************************************************************
	type pcontainer
		type(container),pointer			:: n => null()
	end type pcontainer
!********************************************************************************
	type container2
		integer( kind = k9 )			:: ind1
		real( kind = dp ),allocatable		:: base(:,:),jacobiano(:,:),normalr(:,:)
		type(vmreal1 ),allocatable		:: normal(:),coorreal(:)
		type(mreal2 ),allocatable		:: matrizphi(:,:,:)
	end type container2
!********************************************************************************
	type pcontainer2
		type(container2),pointer		:: n => null()
	end type pcontainer2
!********************************************************************************
	type container3
		type(mcharac),allocatable		:: n(:)
		integer(kind=k9)			:: ind
	end type container3
!********************************************************************************
	type container4
		character(len=20)			:: name
		real( kind = dp )			:: s
		integer( kind = k9 )			:: t
	end type container4
!********************************************************************************
	type container5
		integer( kind = k9 ),allocatable	:: n(:)
		type(container4)			:: s
	end type container5
!********************************************************************************

end Module kinds 
