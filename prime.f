c234567
c 1부터 n사이의 소수의 개수를 구하는 프로그램
      program prime_number
      implicit none
      integer nth, i, j, m, c
      
      print *, 'Get the number of prime numbers btw 1 and n'
      print *, 'put the number of n'
      read (*,*) nth
      
c n개의 크기를 가진 array를 만들기 위해 서브루틴 활용
      call counting(nth)
      
      stop
      end
      
c subroutine
      subroutine counting(n)
      integer n, i, j, mode(n), count
      
      count=0
      
      do i=1,n
          mode(i)=1
      end do
      
      do i=2, n
          do j=2, i-1
              If (mod(i, j) .EQ. 0) mode(i)=mode(i)+1
          end do
          If (mode(i) .EQ. 1) count=count+1
      end do
      
      print *, 'The number of prime number is', count
      
      return
      end