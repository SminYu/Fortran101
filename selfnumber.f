c234567
c 1�̻� 5000�̸��� self-number�� ��
      program self_number
      integer a(1:5000), c(1:5000), s(1:5000), d(1:5000), nn, res, dum
      integer dum2
c initialization
      data nn, res/5000, 0/
      c=0
      s=0
      d=0
c a(i)�� i
      do i=1,nn
          a(i)=i
      end do
c c(i)�� a(i)�� �ڸ���-1 (10**n�� �ִ� n)
      do i=1,5000
          do j=3, 0, -1
              If (i/(10**j) .LT. 10) then
                  c(i) = j
              end if
          end do
      end do
c s(i)�� a(i)�� �� �ڸ����� ��
      do i=1,5000
          dum=i
c dum2�� dum�� 10**j �ڸ���
c s(i) =+ dum2 �Ͽ��� �ڸ����� ���� �����
c dum���� dum2 * 10**j�� ���� ó������ �ݺ�
          do j=c(i),0,-1
              dum2=dum/(10**j)
              s(i)=s(i)+dum2
              dum=dum-dum2*(10**j)
          end do
c d(i)�� s(i) + i (�� �ڸ����� �հ� �ڱ� �ڽ��� ��)
          d(i)=s(i)+i
      end do
c ���� �ٸ� ���� generator�� ���� ��� 0���� �ؼ� ��ġ�� �ʰ� �� 
c generator�� 5000���� ũ�� 0���� �ؼ� �ʿ����� ���� ���� ����
      do i=1,nn
          do j=1,nn
              If ((d(i) .EQ. d(j) .AND. i .NE. j) .OR. (d(j) .GE. 5000))
     ~then
                  d(j)=0
              else
              end if
          end do
      end do
c res�� 1�̻� 5000�̸��� d(i)�� ��
      do i=1,nn
          res=res+d(i)
      end do
c 1�̻� 5000�̸��� ���� �� = ind    
      ind=4999*5000/2
c ind - res�� 5000�̸��� d(i)�� ���� ���� ��
      print *, 'The answer is', ind-res
c ������ 1227365
      stop
      end