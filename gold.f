c234567
      program gold_pot
      integer a(1:100), n, aget, bget, i, awin, bwin, j
      real r(1:10)
      
      open(900, file='result.dat')
      
      awin=0
      bwin=0
      
      do j=1,1000
      
      n=10
      
      call RANDOM_NUMBER(r)
      
      do i=1,10
          a(i)=int(10*r(i))
      end do
      

c a�� b�� ���� ��ȭ�� 0���� �ʱ�ȭ
      aget=0
      bget=0
      
      do i=1,n
          print *, a(i)
      end do

   10 continue
      
          If (n. EQ. 0) then
              goto 20
          end if

      call strat(n, a, aget)

          If (n. EQ. 0) then
              goto 20
          end if

      call strat(n, a, bget)
      
      goto 10
      
  20  continue    
      
      
      if (aget .GT. bget) then
          awin=awin+1
          write (900,*) aget, bget, 'A won'
      else if (bget .GT. aget) then
          bwin=bwin+1
          write (900,*) aget, bget, 'B won'
      else
          write (900,*) aget, bget, 'Draw'
      end if
          
      end do
      
      print *, 'A�� �·�', real(awin)/1000.
      print *, 'B�� �·�', real(bwin)/1000.
      
      stop
      end
      
      
c ���� �����ƾ
      subroutine strat(n, a, get)
      integer a(1:n), b1, bn, n, get, i, m1, mn, c1, cn
      
c 1��(����) ���ý� �ּ�����
      m1=min(a(1)-a(2), a(1)-a(n))
      If (m1 .EQ. (a(1)-a(2))) then
          b1 = a(1)-a(2)
      Else
	    b1 = a(1)-a(n)
      End if
c n��(������) ���ý� �ּ�����
      mn=min(A(n)-A(1), A(n)-A(n-1))
      If (mn .EQ. A(n)-A(1)) then
          bn = A(n)-A(1)
      else
	    bn = A(n)-A(n-1)
      end if
      
c ���ʰ� �������� ��츦 �����ؼ� �ּ������� ū ���� ����
c ���� �� get�� ����
c ���� ����, n���� �����͸� n-1���� �����
      If (b1 .GE. bn) then
          get=get+a(1)
	    do i=1,n-1
		    a(i) = a(i+1)
          end do
c ������ ����, n���� �����͸� ����
      else
          get=get+a(n)
	    A(n) = 0
      end if
      n=n-1
      
      return
      end