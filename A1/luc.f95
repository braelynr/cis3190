      program luc

      implicit none
      integer :: d, i
      integer, dimension(0:7,0:15) :: k
      integer, dimension(0:7,0:7,0:1) :: m
      integer, dimension(0:127) :: key
      integer, dimension(0:127) :: message
      equivalence (k(0,0),key(1)),(m(0,0,0),message(1))

      integer, dimension(0:31) :: kb
      integer, dimension(0:31) :: mb
      write(*,1003)
      read(*,1004) (kb(i),i=0,31)

      write(*,1005)
      read(*,1006) (mb(i),i=0,31)

      call expand(message,mb,32)
      call expand(key,kb,32)

      write(*,1000) (key(i), i=0,127)
      write(*,1001) (message(i), i=0,127)

      d=0
      call lucifer(d,k,m)

      d=1
      call lucifer(d,k,m)

      write(*,1001) (message(i),i=0,127)

      call compress(message,mb,32)
      call compress(key,kb,32)
      write(*,1003)
      write(*,1007) (kb(i),i=0,31)
      write(*,1005)
      write(*,1007) (mb(i),i=0,31)

1000  format(' key '/16(1x,i1))
1001  format(' plain '/16(1x,i1))
1002  format(' cipher '/16(1x,i1))
1003  format(' key ')

1004  format(32z1.1)
1005  format(' plain ')
1006  format(32z1.1)
1007  format(1x,32z1.1)
      end

      subroutine expand(a,b,l)
      implicit none
      integer :: i, j, l, v
      integer, dimension(0:*) :: a
      integer, dimension(0:*) :: b
      do 100 i=0,l-1,1
      v=b(i)
      do 200 j=0,3,1
      a((3-j)+i*4)=mod(v,2)
      v=v/2
200   continue
100   continue
      return
      end

      subroutine compress(a,b,l)
      implicit none
      integer :: i, j, l, v
      integer, dimension(0:*) :: a
      integer, dimension(0:*) :: b
      do 100 i=0,l-1,1
      v=0
      do 200 j=0,3,1
      v=v*2+mod(a(j+i*4),2)
200   continue
      b(i)=v
100   continue
      return
      end

      subroutine lucifer(d,k,m)
      implicit none
      integer :: d, v, h, h0, h1, ii, kc, jj, ks, l, jjj, kk
      integer, dimension(0:7,0:7,0:1) :: m
      integer, dimension(0:7,0:15) :: k
      integer, dimension(0:7) :: o

      integer, dimension(0:7,0:7) :: sw
      integer, dimension(0:7) :: pr
      integer, dimension(0:7) :: tr
      integer, dimension(0:1) :: c
      integer, dimension(0:15) :: s0
      integer, dimension(0:15) :: s1
      equivalence (c(0),h),(c(1),l)

!     diffusion pattern
      data o/7,6,2,1,5,0,3,4/

!     inverse of fixed permutation
      data pr/2,5,4,0,3,1,7,6/

!     S-box permutations
      data s0/12,15,7,10,14,13,11,0,2,6,3,1,9,4,5,8/
      data s1/7,2,14,9,3,11,0,4,12,13,1,10,6,15,8,5/

      h0=0
      h1=1

      kc=0
      if (d == 1) kc=8

      do 100 ii=1,16,1

      if (d==1) kc=mod(kc+1,16)
      ks=kc

      do 200 jj=0,7,1
      l=0
      h=0

      do 400 kk=0,3,1
      l=l*2+m(7-kk,jj,h1)
400   continue
      do 410 kk=4,7,1
      h=h*2+m(7-kk,jj,h1)
410   continue

      v=(s0(l)+16*s1(h))*(1-k(jj,ks))+(s0(h)+16*s1(l))*k(jj,ks)

      do 500 kk=0,7,1
      tr(kk)=mod(v,2)
      v=v/2
500   continue

      do 300 kk=0,7,1
      m(kk,mod(o(kk)+jj,8),h0)=mod(k(pr(kk),kc)+tr(pr(kk))+m(kk,mod(o(kk)+jj,8),h0),2)
300   continue
      if (jj < 7 .or. d == 1) kc=mod(kc+1,16)
200   continue

      jjj=h0
      h0=h1
      h1=jjj
100   continue

      do 700 jj=0,7,1
      do 800 kk=0,7,1
      sw(kk,jj)=m(kk,jj,0)
      m(kk,jj,0)=m(kk,jj,1)
      m(kk,jj,1)=sw(kk,jj)
800   continue
700   continue

      return
      end
