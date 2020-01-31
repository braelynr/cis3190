!
!     Braelyn Rotman
!     1006740
!     Assignment 1 - Fortran
!     Due January 31
!
      program luc
      use hex
      implicit none
!     d is represents if message is to be encypted or deciphered
!     i is an index variable
!     key & message represent the key and message in bit format
!     k & m represent the key and message in byte format
!     kb & mb represent the key and message in hexdigit format
      integer :: d, i, l
      integer, dimension(0:7,0:15) :: k = 0
      integer, dimension(0:7,0:7,0:1) :: m = 0
      integer, dimension(0:127) :: key = 0
      integer, dimension(0:127) :: message = 0
      character(len=11) :: userInput
      equivalence (k(0,0),key(0)),(m(0,0,0),message(0))

      integer, dimension(0:31) :: kb = 0, mb = 0, ciphertext

!     Get message in key from user input
      write(*,1003)
      read(*,1004) (kb(i),i=0,31)

!     Call function to receive word to encrypt
      call readWord(userInput)
!     Call function to convert word to hex
      call word2hex(userInput, mb, l)
!     Print out the converted word
      call printhex(mb, l)


!     convert to bit format
      call expand(message,mb,32)
      call expand(key,kb,32)

!     Optional bit output
!      write(*,1000) (key(i), i=0,127)
!      write(*,1001) (message(i), i=0,127)

!     Encipher
      d=0
      call lucifer(d,k,m)

      write(*,*) ('Encrypted message')
      call compress(message,ciphertext,32)
      write(*,1002)
      write(*,1006) (ciphertext(i),i=0, 31)

!     Decipher
      d=1
      call lucifer(d,k,m)

      write(*,*) ('Decrypted message')
!     Statment for Optional bit output
!      write(*,1001) (message(i),i=0,127)

      call compress(message,mb,32)
      call compress(key,kb,32)
      write(*,1007)
      write(*,1006) (kb(i),i=0,31)
      write(*,1005)
      write(*,1006) (mb(i),i=0,31)

!     Formats for optional bit output
! 1000  format(' key '/16(1x,i1))
! 1001  format(' plain '/16(1x,i1))

1002  format(' ciphertext ')
1003  format(' Please Enter a Hexadecimal Key (A-F, 0-9): ')
1007  format(' key ')
1004  format(32z1.1)
1005  format(' plaintext ')
1006  format(32z1.1)
      end program luc

!     subroutine to expand the message from byte to bit format
      subroutine expand(a,b,l)
      implicit none
!     a is the message in bit format
!     b is the message in byte format
      integer, intent(inout), dimension(0:*) :: a
      integer, intent(in), dimension(0:*) :: b

!     l is the length of b in hexdigits
      integer, intent(in) :: l

      integer :: i, j, v

      do i = 0,l-1,1
          v=b(i)
          do j = 0,3,1
              a((3-j)+i*4)=mod(v,2)
              v=v/2
          end do
      end do
      return
      end subroutine expand

!     subroutine to compress the message from bit to byte format
      subroutine compress(a,b,l)
      implicit none
!     a is the message in bit format
!     b is the message in byte format
      integer, intent(in), dimension(0:*) :: a
      integer, intent(inout), dimension(0:*) :: b

!     l is the length of array b in hexdigits
      integer, intent(in) :: l

      integer :: i, j, v

      do i = 0,l-1,1
          v=0
          do j = 0,3,1
              v=v*2+mod(a(j+i*4),2)
          end do
          b(i)=v
      end do
      return
      end subroutine compress

      subroutine lucifer(d,k,m)
      implicit none
!     declaring the message to be passed in and modified
      integer, intent(inout), dimension(0:7,0:7,0:1) :: m

!     declaring the key to be passed in
      integer, intent(in), dimension(0:7,0:15) :: k

!     d=0 means encrypt, d=1 means decipher
      integer, intent(in) :: d

      integer :: v, h, h0, h1, ii, kc, jj, ks, l, jjj, kk

!     diffusion pattern
      integer, dimension(0:7) :: o = (/7,6,2,1,5,0,3,4/)

      integer, dimension(0:7,0:7) :: sw

!     inverse of fixed permutation
      integer, dimension(0:7) :: pr = (/2,5,4,0,3,1,7,6/)

      integer, dimension(0:7) :: tr
      integer, dimension(0:1) :: c

!     S-box permutations
      integer, dimension(0:15) :: s0 = (/12,15,7,10,14,13,11,0,2,6, &
      3,1,9,4,5,8/), s1 = (/7,2,14,9,3,11,0,4,12,13,1,10,6,15,8,5/)

      equivalence (c(0),h),(c(1),l)

!     pointers to each half of the message
      h0=0
      h1=1

      kc=0
      if (d == 1) then
          kc=8
      end if

      do ii = 1,16,1
          if (d==1) then
              kc=mod(kc+1,16)
          end if
!         ks = the index of the transform control byte
          ks=kc

          do jj = 0,7,1
              l=0
              h=0

!             constructs the integer values of the hexdigits
!                   of one byte of the message
              do kk = 0,3,1
                  l=l*2+m(7-kk,jj,h1)
              end do
              do kk = 4,7,1
                  h=h*2+m(7-kk,jj,h1)
              end do

!             s-box permutation
              v=(s0(l)+16*s1(h))*(1-k(jj,ks))+(s0(h)+16*s1(l))*k(jj,ks)

!             convert v back into bit array
              do kk = 0,7,1
                  tr(kk)=mod(v,2)
                  v=v/2
              end do

!             key interruption and diffusion
              do kk = 0,7,1
                  m(kk,mod(o(kk)+jj,8),h0)=mod(k(pr(kk),kc)+ &
                  tr(pr(kk))+m(kk,mod(o(kk)+jj,8),h0),2)
              end do
              if (jj < 7 .or. d == 1) then
                  kc=mod(kc+1,16)
              end if
          end do

!         swapping halves of the message
          jjj=h0
          h0=h1
          h1=jjj
      end do

!     swap halves of the message content after last round
      do jj = 0,7,1
          do kk = 0,7,1
              sw(kk,jj)=m(kk,jj,0)
              m(kk,jj,0)=m(kk,jj,1)
              m(kk,jj,1)=sw(kk,jj)
          end do
      end do

      return
      end subroutine lucifer
