c234567
c ��ħ�� ��ħ ������ ���� ���
      program clock_angle
      implicit none

      real time, hour, min, sec, hrang, minang, angbtw
      integer d90, d180

      d90=0
      d180=0
      
      do hour=0,23
          do min=0,59
              do sec=0,59
      
c ��ħ�� 60�п� 360�� �����δ�.
c 360��/60�� = 6��/1�� = 6��/60�� = 0.1��/1��
      minang=6.0*min+0.1*sec
      
c ��ħ�� 12�ð��� 360�� �����δ�.
c 360��/12�ð� = 30��/�ð� = 0.5��/�� = 0.5��/60�� = 120����1��/��
c ������ ������ (12�� ����) 0�ú��� �����ѰͰ� ���� (12�ð��� ���� ���)
      If (hour .LT. 12.0) then
          hrang=30.0*hour+0.5*min+(1/120.0)*sec
      else
          hrang=30.0*(hour-12.0)+0.5*min+(1/120.0)*sec
      end if
      
c ��ħ ��ħ ������ ������ ���� ����
      angbtw = abs(hrang-minang)

c �а��� ��� �������� ��ȯ
      If (angbtw .GE. 180.0) angbtw=360.0-angbtw

      
      If (angbtw .EQ. 90) then
          print *, '90 degrees', hour,':',min,':',sec
          d90=d90+1
      else if (angbtw .EQ. 180) then
          print *, '180 degrees', hour,':',min,':',sec
          d180=d180+1
      else
      end if
      
              end do
          end do
      end do
      
      
      print *, 'total 90', d90
      print *, 'total 180', d180
      
      stop
      end