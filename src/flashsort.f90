!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!                                                             c
!     Subroutine Flash1                                       c
!     SORTS ARRAY A WITH N ELEMENTS BY USE OF INDEX VECTOR L  c
!     OF DIMENSION M WITH M ABOUT 0.1 N.                      c
!     Karl-Dietrich Neubert, FlashSort1 Algorithm             c
!     in  Dr. Dobb's Journal Feb.1998,p.123                   c
!                                                             c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine flash1 (A, N, L, M, ind)

   implicit none
   double precision :: a(*), anmin, c1, hold, flash
   integer :: L(*), ind(*), i, n, nmax, m, k, ihold, nmove, j, iflash
   !============================ CLASS FORMATION =====


   do i = 1, n
      ind(i) = i
   end do

   ANMIN=A(1)
   NMAX=1
   DO I=1,N
      IF( A(I).LT.ANMIN) ANMIN=A(I)
      IF( A(I).GT.A(NMAX)) NMAX=I
   END DO

   IF (ANMIN.EQ.A(NMAX)) RETURN
   C1=(M - 1) / (A(NMAX) - ANMIN)
   DO K=1,M
      L(K)=0
   END DO
   DO I=1,N
      K=1 + INT(C1 * (A(I) - ANMIN))
      L(K)=L(K) + 1
   END DO
   DO K=2,M
      L(K)=L(K) + L(K - 1)
   END DO
   HOLD=A(NMAX)
   A(NMAX)=A(1)
   A(1)=HOLD

   ihold = ind(nmax)
   ind(nmax) = ind(1)
   ind(1) = ihold


   !=============================== PERMUTATION =====
   NMOVE=0
   J=1
   K=M
   DO WHILE (NMOVE.LT.N - 1)
      DO WHILE (J.GT.L(K))
         J=J + 1
         K=1 + INT(C1 * (A(J) - ANMIN))
      END DO
      FLASH=A(J)
      iflash=ind(j)

      DO WHILE (.NOT.(J.EQ.L(K) + 1))
         K=1 + INT(C1 * (FLASH - ANMIN))
         HOLD=A(L(K))
         ihold = ind(L(k))
         A(L(K))=FLASH
         ind(L(k)) = iflash
         iflash = ihold
         FLASH=HOLD
         L(K)=L(K) - 1
         NMOVE=NMOVE + 1
      END DO
   END DO

   !========================= STRAIGHT INSERTION =====
   DO I=N-2,1,-1
      IF  (A(I + 1).LT.A(I)) THEN
         HOLD=A(I)
         ihold = ind(i)
         J=I
         DO WHILE  (A(J + 1).LT.HOLD)
            A(J)=A(J + 1)
            ind(j) = ind(j+1)
            J=J + 1
         END DO
         A(J)=HOLD
         ind(j) = ihold
      ENDIF
   END DO

   !=========================== RETURN,END FLASH1 =====
   RETURN
END

