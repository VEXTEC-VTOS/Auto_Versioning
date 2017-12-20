program Auto_versioning


implicit none
    
    character(63) vs_version,line
    character(19) version_line
    Integer(4)  i,d,m,y,nd,ny,nm,dy
    integer::month(12)
    ! open the file that has the last version number (i)
    open(84,file='current.ver')
    call idate(m,d,y)
    dy=6210                     ! number of days from 1/1/2000 until 1/1/2017
    ! if not leap years
    month(1)=31
    month(2)=28
    month(3)=31
    month(4)=30
    month(5)=31
    month(6)=30
    month(7)=31
    month(8)=31
    month(9)=30
    month(10)=31
    month(11)=30
    month(12)=31
    ! this algorithm is applicable until feb/2020
    do ny=2017,y-1,1
        dy=dy+365
    enddo
    do nm=1,m-1,1
        dy=dy+month(nm)
    enddo
    dy=d+dy
    ! read the version number
    read(84,*)i
    ! start creating the resource.rc by copying from resource.txt
    open(85,file='resource.txt')
    open(86,file='resource.rc')
    do while (vs_version.ne.'VS_VERSION_INFO VERSIONINFO')
        read(85,'(63a)')vs_version
        vs_version=trim(vs_version)
        write(86,*)vs_version
    enddo
    i=i+1
    ! write the updated version line in resource.rc
    write(86,1)dy,i
1   format('FILEVERSION 12,17,',i5,',',i5)
    rewind(84)
    write(84,*)i
    close(84)
    read(85,*)
    ! continue copying from resource.txt
    do while(.not.eof(85))
        read(85,'(63a)')line
        line=trim(line)
        write(86,*)line
    enddo
    close(85)
    close(86)
end program