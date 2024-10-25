C     *****************************************************************
C     *****************************************************************

      subroutine evalal(n,x,m,lambda,rho,f,flag)

C     This subroutine computes the objective function when GENCAN is
C     being used stand-alone to solve a unique bound-constrained problem. 
C     When GENCAN is being used in an Augmented Lagrangian framework, 
C     this subroutine must compute the Augmented Lagrangian function.
C
C     On Entry:
C
C     n     integer,
C           number of variables,
C
C     x     double precision x(n),
C           current point,
C
C     m     integer,
C           number of constraints (equalities plus inequalities),
C
C     lambda double precision lambdae(m),
C           current estimation of the Lagrange multipliers,
C
C     rho   double precision rho(m)
C           penalty parameters,
C
C     NOTE: arguments m, lambda and rho are useful when GENCAN is being used
C     for solving the box-constrained subproblems of an Augmented Lagrangian
C     framework. When GENCAN is being used stand-alone for solving a bound-
C     constrained problem, these arguments are dummy arguments.
C
C     On Return
C
C     f     double precision,
C           objective function value at x,
C
C     flag  integer
C           0 means "no errors",
C           1 means "some error occurs in the objective funtion evaluation".

      implicit none

C     SCALAR ARGUMENTS
      integer flag,m,n
      double precision f

C     ARRAY ARGUMENTS
      double precision lambda(m),rho(m),x(n)

C     LOCAL SCALARS

      flag = 0

      call computef(n,x,f)

      end

C     *****************************************************************
C     *****************************************************************

      subroutine evalnal(n,x,m,lambda,rho,g,flag)

C     This subroutine computes the gradient of the objective function 
C     when GENCAN is being used stand-alone to solve a unique bound-
C     constrained problem. When GENCAN is being used in an Augmented
C     Lagrangian framework, this subroutine must compute the gradient of
C     Augmented Lagrangian.
C
C     On Entry:
C
C     n     integer,
C           number of variables,
C
C     x     double precision x(n),
C           current point,
C
C     m     integer,
C           number of constraints (equalities plus inequalities),
C
C     lambda double precision lambdae(m),
C           current estimation of the Lagrange multipliers,
C
C     rho   double precision rho(m)
C           penalty parameters,
C
C     NOTE: arguments m, lambda and rho are useful when GENCAN is being used
C     for solving the box-constrained subproblems of an Augmented Lagrangian
C     framework. When GENCAN is being used stand-alone for solving a bound-
C     constrained problem, these arguments are dummy arguments.
C
C     On Return
C
C     g     double precision g(n),
C           gradient of the objective function at x,
C
C     flag  integer
C           0 means "no errors",
C           1 means "some error occurs in the gradient evaluation".

      implicit none

C     SCALAR ARGUMENTS
      integer flag,m,n

C     ARRAY ARGUMENTS
      double precision g(n),lambda(m),rho(m),x(n)

C     LOCAL SCALARS

      flag = 0

      call computeg(n,x,g)

      end

C     *****************************************************************
C     *****************************************************************

c Modified by L. Martinez (there was an error on the number of
c parameters when calling this subroutine). This subroutine does
c nothing.
c      subroutine evalhd(nind,ind,n,x,m,lambda,rho,d,hd,flag)

      subroutine evalhd(n)

C     This subroutine computes the product of the Hessian matrix times
C     the input vector argument d. If GENCAN is being used stand-alone 
C     to solve a bound-constrained problem, the ''Hessian matrix'' must
C     be the Hessian matrix of the objective function. On the other hand,
C     if GENCAN is being used to solve the bound-constrained subproblems
C     in an Augmented Lagrangian framework, the Hessian matrix must be
C     the Hessian of the Augmented Lagrangian function.
C
C     IMPORTANT: This subroutine does not need to be coded if the user
C     prefers to approximate the Hessian-vector product by incremental 
C     quotients. In this case, it is enough to set the GENCAN input
C     argument htvtype equal to 1 and an internal GENCAN subroutine will
C     be used to compute the approximation. In fact, this is the default
C     GENCAN option. See the GENCAN and EASYGENCAN arguments descriptions
C     for details.
C
C     On Entry:
C
C     nind  integer
C           number of component of the Hessian-vector product that
C           must be computed,
C
C     ind   integer ind(nind)
C           the component that must be computed are ind(1)-th ... ind(nind)-th,
C
C     n     integer,
C           number of variables,
C
C     x     double precision x(n),
C           current point,
C
C     m     integer,
C           number of constraints (equalities plus inequalities),
C
C     lambda double precision lambdae(m),
C           current estimation of the Lagrange multipliers,
C
C     rho   double precision rho(m)
C           penalty parameters,
C
C     d     double precision d(n)
C           vector of the Hessian-vector product.
C
C     NOTE: arguments m, lambda and rho are useful when GENCAN is being used
C     for solving the box-constrained subproblems of an Augmented Lagrangian
C     framework. When GENCAN is being used stand-alone for solving a bound-
C     constrained problem, these arguments are dummy arguments.
C
C     On Return
C
C     hd    double precision g(n),
C           Hessian-vector product,
C
C     flag  integer
C           0 means "no errors",
C           1 means "some error occurs in the gradient evaluation".

      implicit none

C     SCALAR ARGUMENTS
c      integer flag,m,n,nind
      integer n

C     ARRAY ARGUMENTS
c      integer ind(nind)
c      double precision d(n),hd(n),lambda(m),rho(m),x(n)

c      flag = - 1

      end
 
C**************************************************************************

C     Last update of EASYGENCAN: February 18th, 2005.

      subroutine easygencan(n,x,l,u,m,lambda,rho,epsgpsn,maxit,maxfc,
     +trtype,iprint,ncomp,f,g,gpsupn,iter,fcnt,gcnt,cgcnt,inform,wi,wd,
     +delmin)

      implicit none

C     SCALAR ARGUMENTS
      integer cgcnt,fcnt,gcnt,m,maxfc,maxit,n,ncomp,inform,iprint,iter
      double precision epsgpsn,f,gpsupn

C     ARRAY ARGUMENTS
      integer wi(n)
      double precision g(n),l(n),lambda(m),rho(m),u(n),wd(8*n),x(n)

C     This subroutine aims to simplify the use of GENCAN. For this 
C     purpose it gives values to most of the GENCAN arguments and 
C     leaves to the user those arguments which he/she may would like to 
C     set by him/herself.
C
C     The arguments of EASYGENCAN are the input and output arguments of 
C     GENCAN that are supposed to be useful for a common user. The input 
C     arguments are mostly related to basic problem information, like 
C     dimension and bounds, and the initial point. There are also input 
C     arguments related to simple stopping criteria (like norm of the 
C     projected gradient, and maximum number of iterations and 
C     functional evaluations). There are also two input arguments 
C     related to control the amount of information written into the 
C     screen. The output arguments are related to information of the 
C     solution and some few performance measurements. Basically, on 
C     return, EASYGENCAN gives to the user the solution, the objective 
C     functional value and its gradient at the solution, Euclidian and 
C     sup-norm of the projected gradient at the solution, the number of 
C     iterations, functional and gradient evaluations, and Conjugate 
C     Gradient iterations used to reach the solution, and, finally, a 
C     flag that indicates the stopping criterion that was satisfied.
C
C     All the other arguments of GENCAN are setted with its default 
C     values by EASYGENCAN. EASYGENCAN divides the arguments of GENCAN 
C     in two sets. Those that are related to the behaviour of GENCAN are 
C     declared as Fortran parameters (constants). The other arguments of 
C     GENCAN, most of them related to alternative stopping criteria, and 
C     that may depend of, for example, maxit, are declared as local 
C     variables of EASYGENCAN.
C
C     GENCAN arguments that are defined as Fortran parameters in this 
C     subroutine are GENCAN arguments that should not be modified by a 
C     common user. They are arguments that modify the behaviour of 
C     GENCAN and whos values were selected because they are classical 
C     values in some cases or because some numerical experiments seemed 
C     to indicate that they are the best choices.
C
C     GENCAN arguments that are declared as local variables in this 
C     subroutine are GENCAN arguments that may be modified if, with 
C     their suggested values, GENCAN does not give the desired result. 
C     Most of them are related to Conjugate Gradients or to disabled 
C     stopping criteria that may be useful in bad-scaled problems or 
C     problems with not trustable derivatives.
C
C     Finally, this subroutine declares as local variables some 
C     arguments of GENCAN which in fact are output arguments. Most of 
C     them are related to quantities that can be used for statistics 
C     related to the GENCAN performance, like number Spectral Projected 
C     Gradient iterations, Truncated Newton iterations, Conjugate 
C     Gradient iterations, etc. As we assume that this values are not 
C     useful for the common user, this subroutine throw all of them 
C     away.
C
C     We describe below the meaning of the arguments of the EASYGENCAN
C     subroutine. More detailed descriptions as well as the descriptions 
C     of all the other GENCAN arguments that are not arguments of 
C     EASYGENCAN are also described at the begining of the GENCAN 
C     subroutine.
C     
C     On entry:
C
C     n        integer 
C              number of variables
C
C     x        double precision x(n)
C              initial estimation of the solution
C
C     l        double precision l(n)
C              lower bounds on the variables
C
C     u        double precision u(n)
C              upper bounds on the variables
C
C     m        integer
C     lambda   double precision lambda(m)
C     rho      double precision rho(m)
C              These three parameters are not used nor modified by 
C              GENCAN and they are passed as arguments to the user-
C              defined subroutines evalal and evalnal to compute the 
C              objective function and its gradient, respectively. 
C              Clearly, in an Augmented Lagrangian context, if GENCAN is 
C              being used to solve the bound-constrainted subproblems, m 
C              would be the number of constraints, lambda the Lagrange 
C              multipliers approximation and rho the penalty parameters
C
C     epsgpsn  double precision
C              GENCAN stops declaring convergence if it finds a point 
C              whos projected gradient sup-norm is smaller than or equal 
C              to epsgpsn
C
C     maxit    integer
C              GENCAN stops declaring ''maximum number of iteration 
C              achieved'' if the number of iterations exceeds maxit
C
C     maxfc    integer
C              the same as before but with the number of functional 
C              evaluations
C
C     iprint   integer
C              indicates the degree of details of the output generated 
C              by GENCAN. Setting iprint to a value smaller than 2 will 
C              make GENCAN to generate no output at all. An iprint value 
C              greater than or equal to 2 will generate information of 
C              every GENCAN iteration. An iprint value greater than or 
C              equal to 3 will also show information of the Conjugate 
C              Gradient iterations (used to compute the Truncated Newton 
C              direction) and also information related to the line 
C              search procedures in the Spectral Projected Gradient 
C              direction and the Truncated Newton direction.
C
C     ncomp    integer
C              Sometimes, vectors like the current point x, the gradient 
C              of the objective function g, or the search directions 
C              (Spectral Projected Gradient direction or Truncated 
C              Newton direction), among other vector, are showed in the 
C              screen. In such cases, if the problem dimension is large, 
C              to show just a few elements of these vectors may be 
C              preferable. Argument ncomp can be used to indicate how 
C              many array elements must be displayed.
C
C     wi       integer wi(n)
C              integer working space
C
C     wd       double precision wd(8*n)
C              double precision working space
C
C     On return:
C
C     x        double precision x(n)
C              estimation of the solution
C
C     f        double precision
C              objective function value at the solution
C
C     g        double precision g(n)
C              gradient of the objective function at the solution
C
C     gpsupn   double precision
C              sup-norm of the continuous projected gradient
C
C     iter     integer
C              number of iterations used to reach the solution
C
C     fcnt     integer
C              number of functional evaluations
C
C     gcnt     integer
C              number of gradient evaluations
C
C     cgcnt    integer
C              number of Conjugate Gradient iterations
C
C     inform   integer
C              termination criteria. inform equal to 1 means that 
C              GENCAN converged with the sup-norm of the continuous 
C              projected gradient stopping criterion (inform equal to 0
C              means the same but with the Euclidian norm). Other 
C              positive values means that GENCAN stopped by a may be not  
C              successful stopping criteria. A negative value means that 
C              there was an error in the user-defined subroutines that 
C              computes the objective function (subroutine evalal), the 
C              gradient (subroutine evalnal), or the Hessian-vector
C              product (subroutine evalhd). See the GENCAN description 
C              for more details.

C     HERE STARTS THE DESCRIPTION OF SOME GENCAN ARGUMENTS THAT ARE 
C     BEING SETTED INSIDE EASYGENCAN. THE FIRST SET OF ARGUMENTS ARE 
C     THOSE ARGUMENTS THAT WE WILL CALL ''CONSTANTS'' AND THAT, AS THEIR 
C     VALUES ALTER THE BEHAVIOUR OF GENCAN, SHOULD NOT BE MODIFIED BY A 
C     COMMON USER.

C     CONSTANTS FOR GENERAL USES

C     Steps: h = max( steabs, sterel * abs( x ) ) should be a number 
C     such that h is small ( relatively to x ) and x + h is different 
C     from x. So, h is something that can be used a a step for a finite 
C     differences approximation of a partial derivative relative to x.

C     Epsilons: something smaller than max( epsabs, epsrel * abs( x ) )
C     should be considered as ``zero'' when compared with x. It is used, 
C     for example, to detect that a step taken during a line search is 
C     too small.

C     Infinitys: infrel is a big number that may appear in the 
C     calculations. infabs is a number that should never be reached in 
C     the calculations and is used the represent ``infinite''. Detailed 
C     explanations of how are they used are rather cumbersome.

      double precision steabs,sterel,epsabs,epsrel,infabs,infrel
      parameter ( steabs    = 1.0d-10 )
      parameter ( sterel    = 1.0d-07 )
      parameter ( epsabs    = 1.0d-20 )
      parameter ( epsrel    = 1.0d-10 )
      parameter ( infabs    = 1.0d+99 )
      parameter ( infrel    = 1.0d+20 )

C     CONSTANTS FOR CLASSICAL LINE-SEARCH CONDITIONS

C     beta is the constant for the ''beta condition''. We use this 
C     condition to test whether is promising to extrapolate or not.

C     gamma is the constant for the sufficient decrease ''Armijo 
C     condition''.

C     theta is the constant for the ''angle condition''.

C     sigma1 and sigma2 are the constants for the safeguarding quadratic 
C     interpolations. We use them in a rather unusual way. Instead of 
C     discarding a new step anew if it does not belong to the interval 
C     [ sigma1 * aprev, sigma2 * aprev ], we discard it if it does not 
C     belong to the interval [ sigma1, sigma2 * aprev ]. In such a case 
C     we take something similar to ''anew = aprev / 2''.

      double precision beta,gamma,theta,sigma1,sigma2
      parameter ( beta   =   0.5d0 )
      parameter ( gamma  = 1.0d-04 )
      parameter ( theta  = 1.0d-06 )
      parameter ( sigma1 =   0.1d0 )
      parameter ( sigma2 =   0.9d0 )

C     CONSTANTS FOR SPECIFIC PROCEDURES (NOT SO CLASSICAL)

C     In line searches, when interpolating, the step may become so 
C     small that we should declare a line search failure indicating that 
C     direction may not be a descent direction. This decision is never 
C     take before doing at least mininterp interpolations.

C     In line searches, the beta condition (see above) may recommend to
C     extrapolate. We never do more than maxextrap extrapolations.

C     In the line searches, when we need to interpolate and the result 
C     of the quadratic interpolation is rejected, the new step is 
C     computed as anew = aprev / nint. When the beta condition 
C     recommends to extrapolate, we compute anew = aprev * next.

C     When computing the Newton direction by Conjugate Gradients we 
C     never go further an artificial ''trust region''. This ''trust 
C     radius'' is never smaller than delmin.

C     In active set strategies, constants eta is used to decide whether 
C     the current face should be abandoned or not. In particular, the 
C     current face is abandoned when the norm of the internal to face 
C     component of the continuous projected gradient is smaller than 
C     ( 1 - eta ) times the norm of the continuous projected gradient. 
C     In this way, values of eta near 1 makes the method to work hard 
C     inside the faces and values of eta near 0 makes the method to 
C     abandon the faces very quickly.

C     We always use as a first step in a line search procedure along a
C     first order direction the spectral steplength. This steplength 
C     must belong to the interval [lspgmi,lspgma].

      integer maxextrap,mininterp
      parameter ( maxextrap = 100 )
      parameter ( mininterp =   4 )

      double precision nint,next,delmin,eta,lspgma,lspgmi
      parameter ( nint    =   2.0d0 )
      parameter ( next    =   2.0d0 )
c      parameter ( delmin  =   1.d4 )
      parameter ( eta     =   0.9d0 )
      parameter ( lspgma  = 1.0d+10 )
      parameter ( lspgmi  = 1.0d-10 )

C     DIMENSIONS FOR SOME WORKING SPACES

C     In non-monotone line searches, given p, the last p objective 
C     functional values must be stored. For this reason we declare a 
C     vector with pmax double precision elements. So p must be less than
C     or equal to pmax.

C     Sometimes, is the problem is bad scaled, to request a small 
C     gradient norm at the solution may be inadequate. For this reason, 
C     a test to verify if this norm is not decreasing during maxitngp 
C     (MAXimum of ITerations with No Gradient Progress) consecutive 
C     iterations then we stop the method with a warning. As it is not 
C     expected a monotone decreasing of the gradient norm, again, the 
C     norm of the last maxitngp iterations must be saved. For this 
C     purpose, we declare a vector of tmax elements. So maxitngp must 
C     be less than or equal to tmax.
     
      integer tmax
      parameter ( tmax = 1000 )

C     HERE STARTS THE DESCRIPTION OF THE OTHER ARGUMENTS OF GENCAN BEING 
C     SETTED BY EASYGENCAN. THESE ARGUMENTS MAY BE MODIFIED BY A COMMON 
C     USER IF, WITH THEIR SUGGESTED VALUES, GENCAN DOES NOT GIVE THE 
C     EXPECTED RESULT.

C     GENCAN INPUT ARGUMENTS THAT WILL BE SETTED BELOW

      logical nearlyq

      integer cgmaxit,cgscre,gtype,htvtype,maxitnfp,maxitngp,maxitnqmp,
     +        trtype

      double precision cgepsf,cgepsi,cggpnf,delta0,epsgpen,epsnfp,
     +        epsnqmp,fmin

C     GENCAN OUTPUT ARGUMENTS THAT WILL BE DISCARDED

      integer spgfcnt,spgiter,tnexbcnt,tnexgcnt,tnexbfe,tnexgfe,tnfcnt,
     +        tnintcnt,tnintfe,tniter,tnstpcnt

      double precision gpeucn2

C     GENCAN WORKING VECTORS (WHICH DIMENSION IS NOT RELATED TO THE 
C     PROBLEM DIMENSION)

      double precision lastgpns(tmax)

C     ARGUMENTS RELATED TO DERIVATIVES CALCULATIONS

C     gtype indicates in which way the gradient of the objective 
C     function will be computed. If the user have been implemented the 
C     user-supplied evalnal subroutine to compute the gradient of the 
C     objective function then gtype argument must be set to 0 (ZERO) and 
C     the user-supplied evalnal subroutine will be called by GENCAN any 
C     time the gradient would be required.
C
C     The prototype of the evalnal subroutine must be:
C
C         subroutine evalnal(n,x,m,lambda,rho,nal,flag)
C
C         SCALAR ARGUMENTS
C         integer n,m,flag
C
C         ARRAY ARGUMENTS
C         double precision x(n),lambda(m),rho(m),nal(n)
C
C         ''Here must be written the subroutine body that calculates the 
C         n-dimensional gradient vector of the objective function 
C         evaluated at x and saves it in nal. It also must set flag to 0 
C         (ZERO) if the gradient was successfully computed and to any 
C         other value if the gradient vector is not well defined at the 
C         required point x. If GENCAN is been used stand-alone to solve 
C         a unique bound-constrained problem then m, lambda and rho are 
C         dummy arguments. On the other hand, if GENCAN is been used in 
C         an Augmented Lagrangian framework then these arguments should 
C         be used for the number of constraints, the Lagrange 
C         multipliers approximation and the penalty parameters, 
C         respectively.''
C
C         end
C
C     If, on the other hand, the user is not able to provide evalnal 
C     subroutine, gtype argument must be set to 1 (ONE). In this case, 
C     every time GENCAN needs to compute the gradient of the objective 
C     function, an internal subroutine that approximates it by finite-
C     differences will be used (be aware that it maybe very time 
C     consuming). Moreover, note that the evalnal subroutine must still 
C     be present (with an empty body).

      gtype     =        0

C     htvtype indicates in which way the product of the Hessian of the
C     objective function times an arbitrary vector will be computed. If 
C     the user has not been implemented the user-supplied evalhd 
C     subroutine to do this task then htvtype argument must be set to 1 
C     (ONE). In this case an internal subroutine that approximates this 
C     product by incremental quotients will be used. Note that, even in 
C     this case, evalhd subroutine must be present (with an empty body). 
C     This is the default option and the empty-body subroutine follows:
C
C         subroutine evalhd(nind,ind,n,x,m,lambda,rho,d,hd,flag)
C
C         SCALAR ARGUMENTS
C         integer nind,n,m,flag
C
C         ARRAY ARGUMENTS
C         integer ind(nind)
C         double precision d(n),hd(n),lambda(m),rho(m),x(n)
C
C         flag = - 1
C
C         end
C
C     If, on the other hand, the user prefers to implement his/her own 
C     evalhd subroutine then htvtype argument must be set to 0 (ZERO). 
C     In this case, the product of the Hessian times vector d (input 
C     argument of evalhd subroutine) must be saved in vector hd (output 
C     argument of evalhd subroutine). The other arguments description as
C     well as some hints on how to implement your own evalhd subroutine 
C     can be found in the GENCAN arguments description.

C     When ALGENCAN uses GENCAN to solve the subproblems in the classical
C     Augmented Lagrangian framework, ALGENCAN uses its own evalhd
C     subroutine to overcome the lack of continuity of the second 
C     derivatives. So, when GENCAN is being used toghether with ALGENCAN,
C     htvtype must be equal to 0 (ZERO). On the other hand, if GENCAN is
C     being used stand-alone, just set htvtype equal to 1 (ONE) and add 
C     the empty-body subroutine described above.

      htvtype   =        1

C     ARGUMENTS RELATED TO STOPPING CRITERIA

C     Besides the stopping criterion related to the sup-norm of the 
C     continuous projected gradient, there is another stopping criterion 
C     related to its Euclidian norm. So, GENCAN stops the process if it 
C     finds a point at which the Euclidian norm of the continuous 
C     projected gradient is smaller than epsgpen.

      epsgpen   =    0.0d0

C     For an explanation of maxitngp see above the explanation of tmax 
C     in ''DIMENSIONS FOR SOME WORKING SPACES''. Just note that the 
C     value of maxitngp must be less than or equal to tmax.

      maxitngp  =     tmax

C     maxitnfp means MAXimum of allowed number of iterations with No 
C     Progress in the objective functional value. ''Progress'' from one 
C     iteration to the next one refers to ( fnew - fprev ). Since the 
C     begining of the algorithm we save the ''best progress'' and 
C     consider that there was no progress in an iteration if the 
C     progress of this iterations was smaller than epsnfp times the best 
C     progress. Finally, the algorithm stops if there was no progress 
C     during maxitnfp consecutive iterations.

      maxitnfp  =    maxit
      epsnfp    =    0.0d0

C     There is a stopping criterion that stops the method if a point 
C     with a functional value smaller than fmin is found. The idea 
C     behind this stopping criterion is to stop the method if the 
C     objective function is not bounded from below.

      fmin      = 1.0d-05

C     ARGUMENTS RELATED TO CONJUGATE GRADIENTS

C     When computing the Truncated Newton direction by Conjugate 
C     Gradients there is something similar to a ''trust-region radius''. 
C     This trust radius is updated from iteration to iteration depending 
C     on the agreement of the objective function and its quadratic 
C     model. But an initial value for the trust radius is required. If 
C     the user has a good guess for this initial value then it should be 
C     passed to GENCAN using the delta0 arguments. On the other hand, if 
C     delta0 is set to -1, a default value depending on the norm of the 
C     current point will be used.

      delta0    =  - 1.0d0
      delmin  =  1.d-2
c      delta0 = delmin

C     The ''trust-region'' can be like a ball (using Euclidian norm) or 
C     like a box (using sup-norm). This choice can be made using trtype 
C     (TRust region TYPE) argument. trtype equal to 0 means Euclidian 
C     norm and trtype equal to 1 means sup-norm.

      trtype    =        1

C     When the method is far from the solution, it may be not useful to 
C     do a very large effort in computing the Truncated Newton direction 
C     precisely. To avoid it, a fixed maximum number of iterations for 
C     Conjugate Gradients can be given to GENCAN. If the user would like 
C     to choose this maximum number of iterations for Conjugate 
C     Gradient then it should use the cgmaxit arguments. On the other 
C     hand he/she prefers to leave this task to GENCAN then he/she 
C     should set cgmaxit to -1.
 
      cgmaxit   =   -1

C     If the task of deciding the accuracy for computing the Truncated 
C     Newton direction is leaved to GENCAN then a default strategy based 
C     on increasing accuracies will be used. The proximity to the 
C     solution is estimated observing the norm of the projected gradient 
C     at the current point and locating it between that norm at the 
C     initial point and the expected value of that norm at the solution. 
C     Then the accuracy for the Truncated Newton direction of the 
C     current iteration will be computed taking a precision located in 
C     the same relative position with respect to two given values for 
C     the accuracies for the first and the last Truncated Newton 
C     direction calculations. These two accuracies (cgepsi and cgepsf, 
C     respectively) must be given by the user. Moreover, the expected 
C     value of the projected gradient norm at the solution (cggpnf) must
C     also be given by the user who must indicate setting argument 
C     cgscre to 1 or 2 if that norm is the Euclidian or the sup-norm.
      
      cggpnf    =  max( 1.0d-04, max( epsgpen, epsgpsn ) ) 
      cgscre    =        2
      cgepsi    =  1.0d-01
      cgepsf    =  1.0d-05

C     The next two arguments are used for an alternative stopping 
C     criterion for Conjugate Gradients. Conjugate Gradients method is 
C     stopped if the quadratic model makes no progress during maxitnqmp 
C     (MAXimum of ITerations with No Quadratic Model Progress) 
C     consecutive iterations. In this context, ''no progress'' means 
C     that the progress is smaller than epsnqmp (EPSilon to measure the 
C     No Quadratic Model Progress) times the best progress obtained 
C     during the previous iterations.

      epsnqmp   =  1.0d-04
      maxitnqmp =        5

C     Depending on how much the objective function seems to be a 
C     quadratic, function, Conjugate Gradients may take different 
C     decision. So, if the objective function is a quadratic function or 
C     is very similar to a quadratic function then the nearlyq argument 
C     should be set to TRUE, else, it should be set to FALSE. However, 
C     the option with nearlyq equal TRUE never showed good results. 
C     Regarding this unexpected no good performance, rather recently it 
C     was found a bug that affected the behaviour of GENCAN just in this 
C     case (See the April 1st, 2003 modifications report at the end of 
C     this file). So, new experiments setting nearlyq equal TRUE should 
C     be made. 

      nearlyq   =   .false.

C     FINALLY, CALL GENCAN

      call gencan(n,x,l,u,m,lambda,rho,epsgpen,epsgpsn,maxitnfp,epsnfp,
     +maxitngp,fmin,maxit,maxfc,delta0,cgmaxit,cgscre,cggpnf,cgepsi,
     +cgepsf,epsnqmp,maxitnqmp,nearlyq,nint,next,mininterp,maxextrap,
     +gtype,htvtype,trtype,iprint,ncomp,f,g,gpeucn2,gpsupn,iter,fcnt,
     +gcnt,cgcnt,spgiter,spgfcnt,tniter,tnfcnt,tnstpcnt,tnintcnt,
     +tnexgcnt,tnexbcnt,tnintfe,tnexgfe,tnexbfe,inform,wd(1),wd(n+1),
     +wd(2*n+1),wi,lastgpns,wd(3*n+1),eta,delmin,lspgma,lspgmi,theta,
     +gamma,beta,sigma1,sigma2,sterel,steabs,epsrel,epsabs,infrel,
     +infabs)

      end

C     ******************************************************************
C     ******************************************************************

C     Last update of GENCAN or any of its dependencies: 
C
C     February 18th, 2005.
C
C     See report of modifications at the end of this file.

      subroutine gencan(n,x,l,u,m,lambda,rho,epsgpen,epsgpsn,maxitnfp,
     +epsnfp,maxitngp,fmin,maxit,maxfc,udelta0,ucgmaxit,cgscre,cggpnf,
     +cgepsi,cgepsf,epsnqmp,maxitnqmp,nearlyq,nint,next,mininterp,
     +maxextrap,gtype,htvtype,trtype,iprint,ncomp,f,g,gpeucn2,gpsupn,
     +iter,fcnt,gcnt,cgcnt,spgiter,spgfcnt,tniter,tnfcnt,tnstpcnt,
     +tnintcnt,tnexgcnt,tnexbcnt,tnintfe,tnexgfe,tnexbfe,inform,s,y,d,
     +ind,lastgpns,w,eta,delmin,lspgma,lspgmi,theta,gamma,beta,sigma1,
     +sigma2,sterel,steabs,epsrel,epsabs,infrel,infabs)

      implicit none

C     SCALAR ARGUMENTS
      logical nearlyq
      integer cgcnt,cgscre,fcnt,gcnt,gtype,htvtype,inform,iprint,iter,m,
     +        maxextrap,maxfc,maxit,maxitnfp,maxitngp,maxitnqmp,
     +        mininterp,n,ncomp,spgfcnt,spgiter,tnexbcnt,tnexbfe,
     +        tnexgcnt,tnexgfe,tnfcnt,tnintcnt,tnintfe,tniter,tnstpcnt,
     +        trtype,ucgmaxit    
      double precision beta,cgepsf,cgepsi,cggpnf,delmin,epsabs,epsgpen,
     +        epsgpsn,epsnfp,epsnqmp,epsrel,eta,f,fmin,gamma,gpeucn2,
     +        gpsupn,infabs,infrel,lspgma,lspgmi,next,nint,sigma1,
     +        sigma2,steabs,sterel,theta,udelta0

C     ARRAY ARGUMENTS
      integer ind(n)
      double precision d(n),g(n),l(n),lambda(m),lastgpns(0:maxitngp-1),
     +        rho(m),s(n),u(n),w(5*n),x(n),y(n)

C     Solves the box-constrained minimization problem
C
C                         Minimize f(x)
C
C                         subject to 
C 
C                                  l <= x <= u
C     
C     using a method described in 
C
C     E. G. Birgin and J. M. Martinez, ''Large-scale active-set box-
C     constrained optimization method with spectral projected 
C     gradients'', Computational Optimization and Applications 23, pp. 
C     101-125, 2002.  
C
C     Subroutine evalal must be supplied by the user to evaluate the 
C     objective function. The prototype of evalal subroutine must be
C
C           subroutine evalal(n,x,m,lambda,rho,f,flag)
C
C     C     On Entry:
C     C
C     C     n     integer
C     C           number of variables
C     C
C     C     x     double precision x(n)
C     C           current point
C     C
C     C     m     integer
C     C           number of constraints (equalities plus inequalities)
C     C
C     C     lambda double precision lambda(m)
C     C           current estimation of the Lagrange multipliers
C     C
C     C     rho   double precision rho(m)
C     C           penalty parameters
C     C
C     C     NOTE: arguments m, lambda and rho are useful when GENCAN is 
C     C     being used for solving the box-constrained subproblems of an 
C     C     Augmented Lagrangian framework. When GENCAN is being used 
C     C     stand-alone for solving a bound-constrained problem, these 
C     C     arguments are dummy arguments and must be ignored.
C     C
C     C     On Return
C     C
C     C     f     double precision
C     C           objective function value at x
C     C
C     C     flag  integer
C     C           0 means ''no errors''
C     C           any other value means ''there was an error in the 
C     C           objective function calculation''.
C     C
C     C     SCALAR ARGUMENTS
C           integer flag,m,n
C           double precision f
C     
C     C     ARRAY ARGUMENTS
C           double precision lambda(m),rho(m),x(n)
C
C     C     ''Here it should be the body of evalal subroutine that saves 
C     C     in f the objective function value at x. Moreover, it sets 
C     C     flag equal to 0 if the calculation was successfully done and 
C     C     sets flag equal to any other value different from 0 if the 
C     C     objective function is not well defined at the current point 
C     C     x.''
C  
C           end
C
C     Subroutine evalnal to calculate the gradient of the objective 
C     function may be supplied by the user or not, depending on the 
C     value of gtype argument (gtype equal to 0 means that the evalnal 
C     subroutine will be supplied by the user and gtype equal to 1 means 
C     that an internal GENCAN subroutine will be used to estimate the 
C     gradient vector by central finite differences). In any case, a 
C     subroutine named evalnal with the following prototype must 
C     present.
C
C           subroutine evalnal(n,x,m,lambda,rho,g,flag)
C
C     C     On Entry:
C     
C     C     n     integer
C     C           number of variables
C     C
C     C     x     double precision x(n)
C     C           current point
C     C
C     C     m     integer
C     C           number of constraints (equalities plus inequalities)
C     C
C     C     lambda double precision lambda(m)
C     C           current estimation of the Lagrange multipliers
C     C
C     C     rho   double precision rho(m)
C     C           penalty parameters
C     C
C     C     NOTE: arguments m, lambda and rho are useful when GENCAN is 
C     C     being used for solving the box-constrained subproblems of an 
C     C     Augmented Lagrangian framework. When GENCAN is being used 
C     C     stand-alone for solving a bound-constrained problem, these 
C     C     arguments are dummy arguments and must be ignored.
C     C
C     C     On Return
C     C
C     C     g     double precision g(n)
C     C           gradient of the objective function at x
C     C
C     C     flag  integer
C     C           0 means ''no errors'',
C     C           any other value means ''there was an error in the 
C     C           gradient calculation''.
C     C
C     C     SCALAR ARGUMENTS
C           integer flag,m,n
C     
C     C     ARRAY ARGUMENTS
C           double precision g(n),lambda(m),rho(m),x(n)
C
C     C     ''Here it should be the body of evalnal subroutine that 
C     C     saves in g the gradient vector of the objective function at 
C     C     x. Moreover, it sets flag equal to 0 if the calculation was 
C     C     successfully done and sets flag equal to any other value 
C     C     different from 0 if the gradient vector is not well defined 
C     C     at the current point x. If GENCAN gtype argument was setted 
C     C     to 1, i.e., the finite difference approximation provided by 
C     C     GENCAN will be used, then this subroutine must even be 
C     C     present for compilation purpose but it will never be 
C     C     called.''
C  
C      end
C
C     Subroutine evalhd to calculate of the Hessian of the objective 
C     function times a given vector may be supplied by the user or not, 
C     depending on the value of htvtype argument (htvtype equal to 0 
C     means that the evalhd subroutine will be supplied by the user and 
C     htvtype equal to 1 means tha an internal GENCAN subroutine will be
C     used to estimate the product by incremental quotients). In any 
C     case, a subroutine named evalhd with the following prototype must 
C     present.
C
C           subroutine evalhd(nind,ind,n,x,m,lambda,rho,d,hd,flag)
C
C     C     On Entry:
C     C
C     C     nind  integer
C     C           number of component of the Hessian-vector product that
C     C           must be computed
C     C
C     C     ind   integer ind(nind)
C     C           the component that must be computed are ind(1)-th ... 
C     C           ind(nind)-th
C     C
C     C     n     integer
C     C           number of variables
C     C
C     C     x     double precision x(n)
C     C           current point
C     C
C     C     m     integer
C     C           number of constraints (equalities plus inequalities)
C     C
C     C     lambda double precision lambda(m)
C     C           current estimation of the Lagrange multipliers
C     C
C     C     rho   double precision rho(m)
C     C           penalty parameters
C     C
C     C     NOTE: arguments m, lambda and rho are useful when GENCAN is 
C     C     being used for solving the box-constrained subproblems of an 
C     C     Augmented Lagrangian framework. When GENCAN is being used 
C     C     stand-alone for solving a bound-constrained problem, these 
C     C     arguments are dummy arguments and must be ignored.
C     C
C     C     d     double precision d(n)
C     C           vector of the Hessian-vector product
C     C
C     C     On Return
C     C
C     C     hd    double precision g(n)
C     C           Hessian-vector product
C     C
C     C     flag  integer
C     C           0 means ''no errors'',
C     C           any other value means ''there was an error in the 
C     C           product calculation''. Just as an example, as it has
C     C           no sense that an error occurs in a matrix-vector
C     C           product, the error could happen in the Hessian
C     C           calculation. But the possible errors will depend
C     C           on the way this Hessian-vector product is computed
C     C           or approximated.
C
C     C     SCALAR ARGUMENTS
C           integer flag,m,n,nind
C     
C     C     ARRAY ARGUMENTS
C           integer ind(nind)
C           double precision d(n),hd(n),lambda(m),rho(m),x(n)
C     
C     C     ''Here it should be the body of evalhd subroutine that saves 
C     C     in hd the product of the Hessian of the objective function 
C     C     times vector d. Moreover, it sets flag equal to 0 if the 
C     C     calculation was successfully done and sets flag equal to any 
C     C     other value different from 0 if the Hessian matrix is not 
C     C     well defined at the current point x. If GENCAN htvtype 
C     C     argument was setted to 1, i.e., the incremental quotients 
C     C     approximation provided by GENCAN will be used, then this 
C     C     subroutine must even be present for compilation purposes 
C     C     but it will never be called.''
C  
C           end
C
C     In evalhd subroutine, the information about the matrix H must be 
C     passed by means of common declarations. This subroutine must be 
C     coded by the user, taking into account that only nind components 
C     of d are nonnull and that ind is the set of indices of those 
C     components. In other words, the user must write evalhd in such a 
C     way that hd is the vector whose i-th entry is
C 
C               hd(i) = \Sum_{j=1}^{nind} H_{i,ind(j)} d_ind(j)
C
C     Moreover, the only components of hd that must be computed are 
C     those which correspond to the indices ind(1),...,ind(nind). 
C     However, observe that it must be assumed that, in d, the whole 
C     dense vector is present, with its n components, even the null 
C     ones. So, if the user decides to code evalhd without taking into 
C     account the presence of ind and nind, it can be easily done. A 
C     final observation: probably, if nind is close to n, it is not 
C     worthwhile to use ind, due to the cost of accessing the correct 
C     indices. 
C
C     Example: Assume that H is dense. The main steps of evalhd could 
C     be:
C
C          do i = 1,nind
C              indi     = ind(i)
C              hd(indi) = 0.0d0
C              do j = 1,nind
C                  indj     = ind(j)
C                  hd(indi) = hd(indi) + H(indi,indj) * d(indj)
C              end do
C          end do
C
C
C     Description of the GENCAN arguments:
C
C     On Entry
C
C     n        integer 
C              number of variables
C
C     x        double precision x(n)
C              initial estimation of the solution
C
C     l        double precision l(n)
C              lower bounds on the variables
C
C     u        double precision u(n)
C              upper bounds on the variables
C
C     m        integer
C     lambda   double precision lambda(m)
C     rho      double precision rho(m)
C              These three parameters are not used nor modified by 
C              GENCAN and they are passed as arguments to the user-
C              defined subroutines evalal and evalnal to compute the 
C              objective function and its gradient, respectively. 
C              Clearly, in an Augmented Lagrangian context, if GENCAN is 
C              being used to solve the bound-constrainted subproblems, m 
C              would be the number of constraints, lambda the Lagrange 
C              multipliers approximation and rho the penalty parameters
C
C     epsgpen  double precision
C              epsgpen means EPSilon for the Projected Gradient Euclidian
C              Norm. It is a small positive number for declaring 
C              convergence when the Euclidian norm of the continuous 
C              projected gradient is less than or equal to epsgpen
C
C              RECOMMENDED: epsgpen = 1.0d-05
C
C              CONSTRAINTS: epsgpen >= 0.0
C
C     epsgpsn  double precision
C              epsgpsn means EPSilon for the Projected Gradient Sup Norm.
C              It is a small positive number for declaring convergence 
C              when the sup norm of the continuous projected gradient is 
C              less than or equal to epsgpsn
C
C              RECOMMENDED: epsgpsn = 1.0d-05
C
C              CONSTRAINTS: epsgpsn >= 0.0
C
C     maxitnfp integer
C              maxitnfp means MAXimum of ITerations with No Function 
C              Progress. See below for more details.
C
C     epsnfp   double precision
C              epsnfp means EPSilon for No Function Progress. It is a
C              small positive number for declaring ''lack of progress in 
C              the objective function value'' if f(x_k) - f(x_{k+1}) <= 
C              epsnfp * max{ f(x_j) - f(x_{j+1}, j < k } during maxitnfp 
C              consecutive iterations. This stopping criterion may be 
C              inhibited setting maxitnfp equal to maxit.
C
C              RECOMMENDED: maxitnfp = 5 and epsnfp = 1.0d-02
C
C              CONSTRAINTS: maxitnfp >= 1 and epsnfp >= 0.0
C
C     maxitngp integer
C              maxitngp means MAXimum of ITerations with No Gradient
C              Progress. If the order of the Euclidian norm of the 
C              continuous projected gradient did not change during 
C              maxitngp consecutive iterations then the execution stops. 
C
C              RECOMMENDED: maxitngp = 10
C
C              CONSTRAINTS: maxitngp >= 1
C
C     fmin     double precision
C              function value for the stopping criteria f <= fmin
C
C              There is a stopping criterion that stops GENCAN if a 
C              point with a functional value smaller than fmin is found. 
C              The idea behind this stopping criterion is to stop the 
C              method if the objective function is not bounded from 
C              below.
C
C              RECOMMENDED: fmin = - infabs
C
C              CONSTRAINTS: there are no constraints for this argument
C
C     maxit    integer
C              maximum number of allowed iterations
C
C              RECOMMENDED: maxit = 1000
C
C              CONSTRAINTS: maxit >= 0
C
C     maxfc    integer
C              maximum allowed number of functional evaluations
C
C              RECOMMENDED: maxfc = 5 * maxit
C
C              CONSTRAINTS: maxfc >= 1
C
C     udelta0  double precision
C              initial ''trust-radius'' for Conjugate Gradients. The 
C              default value max( delmin, 0.1 * max( 1, ||x|| ) ) is 
C              used if the user sets udelta0 <= 0. 
C
C              RECOMMENDED: udelta0 = - 1.0
C
C              CONSTRAINTS: there are no constraints for this argument
C
C     ucgmaxit integer
C              maximum allowed number of iterations for each run of the 
C              Conjugate Gradient subalgorithm
C
C              The default values for this argument is max( 1, 10 * 
C              log( nind ) ), where nind is the number of free 
C              variables, and it will be used if the user sets ucgmaxit 
C              to any non-positive value. 
C
C          RECOMMENDED: ucgmaxit = - 1
C
C          CONSTRAINTS: there are no constraints for this argument
C
C     cgscre   integer
C              See below
C
C     cggpnf   double precision
C              cgscre means conjugate gradient stopping criterion 
C              relation, and cggpnf means Conjugate Gradients projected 
C              gradient final norm. Both are related to a stopping 
C              criterion of Conjugate Gradients. This stopping criterion 
C              depends on the norm of the residual of the linear system. 
C              The norm of the residual should be less or equal than a 
C              ''small'' quantity which decreases as we are 
C              approximating the solution of the minimization problem 
C              (near the solution, better the truncated-Newton direction 
C              we aim). Then, the log of the required accuracy requested 
C              to Conjugate Gradient has a linear dependence on the log 
C              of the norm of the continuous projected gradient. This 
C              linear relation uses the squared Euclidian norm of the 
C              projected gradient if cgscre is equal to 1 and uses the 
C              sup-norm if cgscre is equal to 2. In addition, the 
C              precision required to CG is equal to cgepsi (conjugate 
C              gradient initial epsilon) at x0 and cgepsf (conjugate 
C              gradient final epsilon) when the Euclidian- or sup-norm 
C              of the projected gradient is equal to cggpnf (conjugate 
C              gradients projected gradient final norm) which is an 
C              estimation of the value of the Euclidian- or sup-norm of 
C              the projected gradient at the solution.
C
C              RECOMMENDED: cgscre = 1, cggpnf = epsgpen; or
C                           cgscre = 2, cggpnf = epsgpsn.
C
C              CONSTRAINTS:  allowed values for cgscre are just 1 or 2
C                            cggpnf >= 0.0
C
C     cgepsi   double precision
C              See below
C
C     cgepsf   double precision
C              small positive numbers for declaring convergence of the 
C              Conjugate Gradients subalgorithm when ||r||_2 < cgeps * 
C              ||rhs||_2, where r is the residual and rhs is the right 
C              hand side of the linear system, i.e., CG stops when the 
C              relative error of the solution is smaller than cgeps. 
C
C              cgeps varies from cgepsi to cgepsf in a way that depends 
C              on cgscre as follows:
C
C              i) CASE cgscre = 1: log10(cgeps^2) depends linearly on 
C              log10(||g_P(x)||_2^2) which varies from ||g_P(x_0)||_2^2 
C              to epsgpen^2
C
C              ii)  CASE cgscre = 2: log10(cgeps) depends linearly on 
C              log10(||g_P(x)||_inf) which varies from ||g_P(x_0)||_inf 
C              to epsgpsn
C
C              RECOMMENDED: cgepsi = 1.0d-01, cgepsf = 1.0d-05
C
C              CONSTRAINTS: cgepsi >= cgepsf >= 0.0
C
C     epsnqmp  double precision
C              See below
C
C     maxitnqmp integer
C              This and the previous argument are used for a stopping 
C              criterion of the Conjugate Gradients subalgorithm. If the 
C              progress in the quadratic model is smaller than fraction 
C              of the best progress ( epsnqmp * bestprog ) during 
C              maxitnqmp consecutive iterations then CG is stopped 
C              declaring ''not enough progress of the quadratic model''.
C
C              RECOMMENDED: epsnqmp = 1.0d-04, maxitnqmp = 5
C
C              CONSTRAINTS: epsnqmp >= 0.0, maxitnqmp >= 1.
C
C     nearlyq  logical
C              If the objective function is (nearly) quadratic, use the 
C              option nearlyq = TRUE. Otherwise, keep the default 
C              option.
C
C              If, in an iteration of CG we find a direction d such that 
C              d^T H d <= 0 then we take the following decision:
C
C              (i) If nearlyq = TRUE then we take direction d and try to 
C              go to the boundary choosing the best point among the two 
C              points at the boundary and the current point. 
C
C              (ii) If nearlyq = FALSE then we stop at the current point.
C
C              Moreover, if the objective function is quadratic more 
c              effort is due in computing the Truncated Newton direction.
C
C              RECOMMENDED: nearlyq = FALSE
C
C              CONSTRAINTS: allowed values are just TRUE or FALSE.
C
C     nint     double precision
C              Constant for the interpolation. See the description of 
C              sigma1 and sigma2 above. Sometimes, in a line search, we 
C              take the new trial step as the previous one divided by 
C              nint
C
C              RECOMMENDED: nint = 2.0
C
C              CONSTRAINTS: nint > 1.0.
C
C     next     double precision
C              Constant for the extrapolation. When extrapolating we 
C              try alpha_new = alpha * next
C
C              RECOMMENDED: next = 2.0
C
C              CONSTRAINTS: next > 1.0
C
C     mininterp integer
C              Constant for testing if, after having made at least 
C              mininterp interpolations, the steplength is too small. In
C              that case, failure of the line search is declared (may be 
C              the direction is not a descent direction due to an error 
C              in the gradient calculations). Use mininterp greater 
C              than or equal to maxfc for inhibit this stopping 
C              criterion
C
C              RECOMMENDED: mininterp = 4 
C
C              CONSTRAINTS: mininterp >= 1
C
C     maxextrap integer
C              Constant to limit the number of extrapolations in the 
C              Truncated Newton direction.
C
C              RECOMMENDED: maxextrap = 100 
C
C              CONSTRAINTS: maxextrap >= 0
C
C     gtype    integer
C              gtype indicates in which way the gradient of the 
C              objective function will be computed. If the user have 
C              been implemented the user-supplied evalnal subroutine to 
C              compute the gradient of the objective function then 
C              gtype argument must be set to 0 (ZERO) and the user-
C              supplied evalnal subroutine will be called by GENCAN any 
C              time the gradient would be required.
C
C                    subroutine evalnal(n,x,m,lambda,rho,g,flag)
C
C              C     On Entry:
C     
C              C     n     integer,
C              C           number of variables,
C              C
C              C     x     double precision x(n),
C              C           current point,
C              C
C              C     m     integer,
C              C           number of constraints (equalities plus 
C              C           inequalities),
C              C
C              C     lambda double precision lambda(m),
C              C           current estimation of the Lagrange 
C              C           multipliers,
C              C
C              C     rho   double precision rho(m)
C              C           penalty parameters,
C              C
C              C     NOTE: arguments m, lambda and rho are useful when 
C              C     GENCAN is being used for solving the box-
C              C     constrained subproblems of an Augmented Lagrangian 
C              C     framework. When GENCAN is being used stand-alone 
C              C     for solving a bound-constrained problem, these 
C              C     arguments are dummy arguments.
C              C
C              C     On Return
C              C
C              C     g     double precision g(n),
C              C           gradient of the objective function at x,
C              C
C              C     flag  integer
C              C           0 means ''no errors'',
C              C           1 means ''some error occurs in the gradient 
C              C             evaluation''.
C              C
C              C     SCALAR ARGUMENTS
C                    integer flag,m,n
C     
C              C     ARRAY ARGUMENTS
C                    double precision g(n),lambda(m),rho(m),x(n)
C
C              C     ''Here it should be the body of evalnal subroutine 
C              C     that saves in g the gradient vector of the 
C              C     objective at x. Moreover, it sets flag equal to 0 
C              C     if the calculation was successfully done and sets 
C              C     flag equal to any other value different from 0 if 
C              C     the gradient vector is not well defined at the 
C              C     current point x. If GENCAN gtype argument was 
C              C     setted to 1, i.e., the finite difference 
C              C     approximation provided by GENCAN will be used, then 
C              C     this subroutine must even be present for 
C              C     compilation purposes but it will never be called.''
C  
C               end
C
C              If, on the other hand, the user is not able to provide 
C              evalnal subroutine, gtype argument must be set to 1 
C              (ONE). In this case, every time GENCAN needs to compute 
C              the gradient of the objective function, an internal 
C              subroutine that approximates it by finite-differences 
C              will be used (be aware that it maybe very time 
C              consuming). Moreover, note that the evalnal subroutine 
C              must still be present (with an empty body).
C
C              RECOMMENDED: gtype = 0 (provided you have the evalg 
C                           subroutine)
C
C              CONSTRAINTS: allowed values are just 0 or 1.
C
C     htvtype  integer
C              htvtype indicates in which way the product of the Hessian 
C              of the objective function times an arbitrary vector will be 
C              computed. If the user has not been implemented the user-
C              supplied evalhd subroutine to do this task then htvtype 
C              argument must be set to 1 (ONE). In this case an internal 
C              subroutine that approximates this product by incremental 
C              quotients will be used. Note that, even in this case, 
C              evalhd subroutine must be present (with an empty body). 
C              This is the default option and the empty-body subroutine 
C              follows:
C
C              subroutine evalhd(nind,ind,n,x,m,lambda,rho,d,hd,flag)
C
C              C     SCALAR ARGUMENTS
C                    integer nind,n,m,flag
C
C              C     ARRAY ARGUMENTS
C                    integer ind(nind)
C                    double precision x(n),lambda(m),rho(m),d(n),hd(n) 
C
C                    flag = - 1
C
C                    end
C
C              If, on the other hand, the user prefers to implement his/
C              her own evalhd subroutine then htvtype argument must be 
C              set to 0 (ZERO). In this case, the product of the Hessian 
C              times vector d (input argument of evalhd subroutine) must 
C              be saved in vector hd (output argument of evalhd 
C              subroutine). The other arguments description as well as 
C              some hints on how to implement your own evalhd subroutine 
C              can be found in the GENCAN arguments description.
C
C              RECOMMENDED: htvtype = 1
C
C              (you take some risk using this option but, unless you 
C              have a good evalhd subroutine, incremental quotients is a 
C              very cheap option)
C
C              CONSTRAINTS: allowed values are just 0 or 1.
C
C     trtype   integer
C              Type of Conjugate Gradients ''trust-radius''. trtype 
C              equal to 0 means Euclidian-norm trust-radius and trtype 
C              equal to 1 means sup-norm trust radius
C
C              RECOMMENDED: trtype = 0
C
C              CONSTRAINTS: allowed values are just 0 or 1.
C
C     iprint   integer
C              Commands printing. Nothing is printed if iprint is 
C              smaller than 2. If iprint is greater than or equal to 
C              2, GENCAN iterations information is printed. If iprint 
C              is greater than or equal to 3, line searches and 
C              Conjugate Gradients information is printed.
C
C              RECOMMENDED: iprint = 2
C
C              CONSTRAINTS: allowed values are just 2 or 3.
C
C     ncomp    integer
C              This constant is just for printing. In a detailed 
C              printing option, ncomp component of some vectors will be 
C              printed
C
C              RECOMMENDED: ncomp = 5
C
C              CONSTRAINTS: ncomp >= 0
C
C     s        double precision s(n)
C     y        double precision y(n)
C     d        double precision d(n)
C     ind      integer ind(n)
C     lastgpns double precision lastgpns(maxitngp)
C     w        double precision w(5*n)
C              working vectors
C
C     eta      double precision
C              Constant for deciding abandon the current face or not. We 
C              abandon the current face if the norm of the internal 
C              gradient (here, internal components of the continuous 
C              projected gradient) is smaller than ( 1 - eta ) times the 
C              norm of the continuous projected gradient. Using eta = 
C              0.9 is a rather conservative strategy in the sense that 
C              internal iterations are preferred over SPG iterations. 
C
C              RECOMMENDED: eta = 0.9
C
C              CONSTRAINTS: 0.0 < eta < 1.0
C
C     delmin   double precision
C              Smaller Conjugate Gradients ''trust radius'' to compute 
C              the Truncated Newton direction
C
C              RECOMMENDED: delmin = 0.1
C
C              CONSTRAINTS: delmin > 0.0
C
C     lspgmi   double precision
C              See below
C
C     lspgma   double precision
C              The spectral steplength, called lamspg, is projected onto 
C              the box [lspgmi,lspgma] 
C
C              RECOMMENDED: lspgmi = 1.0d-10 and lspgma = 1.0d+10
C 
C              CONSTRAINTS: lspgma >= lspgmi > 0.0
C
C     theta    double precision
C              Constant for the angle condition, i.e., at iteration k we 
C              need a direction dk such that <gk,dk> <= - theta 
C              ||gk||_2 ||dk||_2, where gk is \nabla f(xk)
C
C              RECOMMENDED: theta = 10^{-6}
C
C              CONSTRAINTS: 0.0 < theta < 1.0
C
C     gamma    double precision
C              Constant for the Armijo criterion
C              f(x + alpha d) <= f(x) + gamma * alpha * <g,d>
C
C              RECOMMENDED: gamma = 1.0d-04
C
C              CONSTRAINTS: 0.0 < gamma < 0.5.
C
C     beta     double precision
C              Constant for the beta condition <dk, g(xk + dk)>  < beta 
C              * <dk,gk>. If (xk + dk) satisfies the Armijo condition 
C              but does not satisfy the beta condition then the point is 
C              accepted, but if it satisfied the Armijo condition and 
C              also satisfies the beta condition then we know that there 
C              is the possibility for a successful extrapolation
C
C              RECOMMENDED: beta = 0.5
C
C              CONSTRAINTS: 0.0 < beta < 1.0.
C
C     sigma1   double precision
C              See below
C
C     sigma2   double precision
C              Constant for the safeguarded interpolation. If alpha_new 
C              is not inside the interval [sigma1, sigma * alpha] then 
C              we take alpha_new = alpha / nint
C
C              RECOMMENDED: sigma1 = 0.1 and sigma2 = 0.9
C
C              CONSTRAINTS: 0 < sigma1 < sigma2 < 1.
C
C     sterel   double precision
C              See below
C
C     steabs   double precision
C              This constants mean a ''relative small number'' and ''an 
C              absolute small number'' for the increments in finite 
C              difference approximations of derivatives
C
C              RECOMMENDED: epsrel = 1.0d-07 and epsabs = 1.0d-10 
C
C              CONSTRAINTS: sterel >= steabs > 0
C
C     epsrel   double precision
C              See below
C
C     epsabs   double precision
C              See below
C
C     infrel   double precision
C              See below
C
C     infabs   double precision
C              This four constants mean a ''relative small number'', 
C              ''an absolute small number'', ''a relative large number'' 
C              and ''an absolute large number''. Basically, a quantity A 
C              is considered negligible with respect to another quantity 
C              B if |A| < max ( epsrel * |B|, epsabs ) 
C
C              RECOMMENDED: epsrel = 1.0d-10, epsabs = 1.0d-20, 
C                           infrel = 1.0d+20, infabs = 1.0d+99
C
C              CONSTRAINTS: epsrel >= epsabs >= 0.0
C                           infabs >= infrel >= 0.0
C
C     On Return
C
C     x        double precision x(n)
C              Final estimation to the solution
C
C     f        double precision
C              Function value at the final estimation 
C
C     g        double precision g(n)
C              Gradient at the final estimation
C
C     gpeucn2  double precision
C              Squared Euclidian norm of the continuous projected 
C              gradient at the final estimation
C
C     gpsupn   double precision
C              the same as before but with sup-norm
C
C     iter     integer
C              number of iterations
C
C     fcnt     integer
C              number of function evaluations   
C
C     gcnt     integer
C              number of gradient evaluations   
C
C     cgcnt    integer
C              number of Conjugate Gradients iterations   
C
C     spgiter  integer
C              number of Spectral Projected Gradient iterations
C
C     spgfcnt  integer
C              number of functional evaluations along Spectral Projected
C              Gradient directions
C
C     tniter   integer
C              number of Truncated-Newton iterations
C
C     tnfcnt   integer
C              number of functional evaluations along Truncated-Newton
C              directions
C
C     tnintcnt integer
C              number of times a backtracking in a Truncated-Newton
C              direction was needed
C
C     tnexgcnt integer
C              number of times an extrapolation in a Truncated-Newton
C              direction successfully decreased the objective funtional
C              value
C
C     tnexbcnt integer
C              number of times an extrapolation was aborted in the first
C              extrapolated point by an increase in the objective 
C              functional value
C
C     tnstpcnt integer
C              number of times the Newton point was accepted (without
C              interpolations nor extrapolations)
C
C     tnintfe  integer
C              number of functional evaluations used in interpolations 
C              along Truncated-Newton directions
C
C     tnexgfe  integer
C              number of functional evaluations used in successful 
C              extrapolations along Truncated-Newton directions
C
C     tnexbfe  integer
C              number of functional evaluations used in unsuccessful 
C              extrapolations along Truncated-Newton directions
C
C     inform   integer
C              This output parameter tells what happened in this 
C              subroutine, according to the following conventions:
C 
C              0 = convergence with small Euclidian norm of the 
C                  continuous projected gradient (smaller than epsgpen);
C
C              1 = convergence with small sup-norm of the continuous 
C                  projected gradient (smaller than epsgpsn);
C
C              2 = the algorithm stopped by ''lack of progress'', that 
C                  means that f(xk) - f(x_{k+1}) <= epsnfp * 
C                  max{ f(x_j) - f(x_{j+1}, j < k } during maxitnfp 
C                  consecutive iterations. If desired, set maxitnfp 
C                  equal to maxit to inhibit this stopping criterion.
C
C              3 = the algorithm stopped because the order of the 
C                  Euclidian norm of the continuous projected gradient 
C                  did not change during maxitngp consecutive 
C                  iterations. Probably, we are asking for an 
C                  exaggerated small norm of continuous projected 
C                  gradient for declaring convergence. If desired, set
C                  maxitngp equal to maxit to inhibit this stopping 
C                  criterion.
C
C              4 = the algorithm stopped because the functional value 
c                  is very small (smaller than fmin). If desired, set 
C                  fmin equal to minus infabs to inhibit this stopping 
C                  criterion.
C
C              6 = too small step in a line search. After having made at 
C                  least mininterp interpolations, the steplength 
C                  becames small. ''small steplength'' means that we are 
C                  at point x with direction d and step alpha, and 
C
C                  alpha * ||d||_infty < max( epsabs, epsrel * 
C                  ||x||_infty ). 
C 
C                  In that case failure of the line search is declared 
C                  (may be the direction is not a descent direction due 
C                  to an error in the gradient calculations). If 
C                  desired, set mininterp equal to maxfc to inhibit this 
C                  stopping criterion.
C
C              7 = it was achieved the maximum allowed number of 
C                  iterations (maxit);
C
C              8 = it was achieved the maximum allowed number of 
C                  function evaluations (maxfc);
C
C            < 0 = error in evalal, evalnal or evalhd subroutines.

C     LOCAL SCALARS
      character * 3 ittype
      integer cgiter,cgmaxit,fcntprev,i,infotmp,itnfp,nind,nprint,
     +        rbdind,rbdtype,tnexbprev,tnexgprev,tnintprev
      double precision acgeps,amax,amaxx,bestprog,bcgeps,cgeps,currprog,
     +        delta,epsgpen2,fprev,gieucn2,gpeucn20,gpi,gpnmax,gpsupn0,
     +        kappa,lamspg,ometa2,sts,sty,xnorm
      logical packmolprecision

C     ==================================================================
C     Initialization
C     ==================================================================

C     Set some initial values:

C     counters,
      iter     =  0
      fcnt     =  0
      gcnt     =  0
      cgcnt    =  0

      spgiter  =  0
      spgfcnt  =  0

      tniter   =  0
      tnfcnt   =  0

      tnstpcnt =  0
      tnintcnt =  0
      tnexgcnt =  0
      tnexbcnt =  0

      tnintfe  =  0
      tnexgfe  =  0
      tnexbfe  =  0

C     just for printing,
      nprint   = min0( n, ncomp )

C     for testing convergence,
      epsgpen2 = epsgpen ** 2

C     for testing whether to abandon the current face or not,
C     (ometa2 means '(one minus eta) squared')
      ometa2   = ( 1.0d0 - eta ) ** 2

C     for testing progress in f, and
      fprev    = infabs
      bestprog =  0.0d0
      itnfp    =      0

C     for testing progress in the projected gradient norm.
      do i = 0,maxitngp - 1
          lastgpns(i) = infabs
      end do

C     Print problem information

      if( iprint .ge. 3 ) then
          write(*, 977) n
          write(*, 978) nprint,(l(i),i=1,nprint)
          write(*, 979) nprint,(u(i),i=1,nprint)
          write(*, 980) nprint,(x(i),i=1,nprint)

          write(10,977) n
          write(10,978) nprint,(l(i),i=1,nprint)
          write(10,979) nprint,(u(i),i=1,nprint)
          write(10,980) nprint,(x(i),i=1,nprint)
      end if

C     Project initial guess. If the initial guess is infeasible, 
C     projection puts it into the box.

      do i = 1,n
          x(i) = max( l(i), min( x(i), u(i) ) )
      end do

C     Compute x Euclidian norm

      xnorm = 0.0d0
      do i = 1,n
          xnorm = xnorm + x(i) ** 2
      end do
      xnorm = sqrt( xnorm )

C     Compute function and gradient at the initial point

      call evalal(n,x,m,lambda,rho,f,inform)

c LM: Added packmolprecision function test, for Packmol

      if ( packmolprecision(n,x) ) then
        if(iprint.gt.0) then
          write(*,780)
780       format('  Current point is a solution.') 
        end if
        return
      end if

      fcnt = fcnt + 1

      if ( inform .lt. 0 ) then

          if ( iprint .ge. 3 ) then
              write(*, 1000) inform
              write(10,1000) inform
          end if

          return
      end if

      if ( gtype .eq. 0 ) then
          call evalnal(n,x,m,lambda,rho,g,inform)
      else ! if ( gtype .eq. 1 ) then
          call evalnaldiff(n,x,m,lambda,rho,g,sterel,steabs,inform)
      end if
      gcnt = gcnt + 1

      if ( inform .lt. 0 ) then

          if ( iprint .ge. 3 ) then
              write(*, 1000) inform
              write(10,1000) inform
          end if

          return
      end if

C     Compute continuous-project-gradient Euclidian and Sup norms,
C     internal gradient Euclidian norm, and store in nind the number of
C     free variables and in array ind their identifiers.

      nind    = 0
      gpsupn  = 0.0d0
      gpeucn2 = 0.0d0
      gieucn2 = 0.0d0
      do i = 1,n
          gpi     = min( u(i), max( l(i), x(i) - g(i) ) ) - x(i)
          gpsupn  = max( gpsupn, abs( gpi ) )
          gpeucn2 = gpeucn2 + gpi ** 2
          if ( x(i) .gt. l(i) .and. x(i) .lt. u(i) ) then
              gieucn2   = gieucn2 + gpi ** 2
              nind      = nind + 1
              ind(nind) = i
          end if
      end do

C     Compute a linear relation between gpeucn2 and cgeps2, i.e.,
C     scalars a and b such that 
c
C         a * log10(||g_P(x_0)||_2^2) + b = log10(cgeps_0^2) and
c
C         a * log10(||g_P(x_f)||_2^2) + b = log10(cgeps_f^2),
c
C     where cgeps_0 and cgeps_f are provided. Note that if 
C     cgeps_0 is equal to cgeps_f then cgeps will be always 
C     equal to cgeps_0 and cgeps_f.

C     We introduce now a linear relation between gpsupn and cgeps also.

c LM: changed to avoid error with gpsupn=0
      call gp_ieee_signal1(gpsupn, acgeps, bcgeps, cgepsf,
     +      cgepsi, cggpnf)
c      if ( gpsupn .gt. 0.d0 ) then
c         acgeps = log10( cgepsf / cgepsi ) / log10( cggpnf / gpsupn )
c         bcgeps = log10( cgepsi ) - acgeps * log10( gpsupn )
c      else
c         acgeps = 0.0d0
c         bcgeps = cgepsf
c      end if

c      if ( cgscre .eq. 1 ) then
c          acgeps = 2.0d0 * log10( cgepsf / cgepsi ) / 
c     +                     log10( cggpnf ** 2 / gpeucn2 )
c          bcgeps = 2.0d0 * log10( cgepsi ) - acgeps * log10( gpeucn2 )
c      else ! if ( cgscre .eq. 2 ) then
c          acgeps = log10( cgepsf / cgepsi ) / log10( cggpnf / gpsupn )
c          bcgeps = log10( cgepsi ) - acgeps * log10( gpsupn )
c      end if 

C     And it will be used for the linear relation of cgmaxit

      gpsupn0  = gpsupn
      gpeucn20 = gpeucn2

C     Print initial information

c LM: progress bar for packmol
      call printbar(iprint, iter, maxit)

      if( iprint .ge. 3 ) then
          write(*, 981) iter
          write(*, 985) nprint,(x(i),i=1,nprint)
          write(*, 986) nprint,(g(i),i=1,nprint)
          write(*, 987) nprint,(min(u(i),max(l(i),x(i)-g(i)))-x(i),i=1,
     +    nprint)
          write(*, 988) min0(nprint,nind),nind,(ind(i),i=1,min0(nprint,
     +    nind))
          write(*, 1002) f,sqrt(gpeucn2),sqrt(gieucn2),gpsupn,nind,n,
     +    spgiter,tniter,fcnt,gcnt,cgcnt

          write(10,981) iter
          write(10,985) nprint,(x(i),i=1,nprint)
          write(10,986) nprint,(g(i),i=1,nprint)
          write(10,987) nprint,(min(u(i),max(l(i),x(i)-g(i)))-x(i),i=1,
     +    nprint)
          write(10,988) min0(nprint,nind),nind,(ind(i),i=1,min0(nprint,
     +    nind))
          write(10,1002) f,sqrt(gpeucn2),sqrt(gieucn2),gpsupn,nind,n,
     +    spgiter,tniter,fcnt,gcnt,cgcnt
      end if

C     ==================================================================
C     Main loop
C     ==================================================================
      
 100  continue

C     ==================================================================
C     Test stopping criteria
C     ==================================================================

c LM: Added packmolprecision function test, for Packmol

      if ( packmolprecision(n,x) ) then
        goto 500
      end if

C     Test whether the continuous-projected-gradient Euclidian norm
C     is small enough to declare convergence

      if ( gpeucn2 .le. epsgpen2 ) then
          inform = 0

          if ( iprint .ge. 3 ) then
              write(*, 990) inform,epsgpen
              write(10,990) inform,epsgpen
          end if

          go to 500
      end if

C     Test whether the continuous-projected-gradient Sup norm
C     is small enough to declare convergence

      if ( gpsupn .le. epsgpsn ) then
          inform = 1

          if ( iprint .ge. 3 ) then
              write(*, 991) inform,epsgpsn
              write(10,991) inform,epsgpsn
          end if

          go to 500
      end if

C     Test whether we performed many iterations without good progress
C     of the functional value

      currprog = fprev - f
      bestprog = max( currprog, bestprog )

      if ( currprog .le. epsnfp * bestprog ) then

          itnfp = itnfp + 1

          if ( itnfp .ge. maxitnfp ) then
              inform = 2

              if ( iprint .ge. 3 ) then
                  write(*, 992) inform,epsnfp,maxitnfp
                  write(10,992) inform,epsnfp,maxitnfp
              end if

              go to 500
          endif

      else
          itnfp = 0
      endif

C     Test whether we have performed many iterations without good 
C     reduction of the euclidian-norm of the projected gradient

      gpnmax = 0.0d0
      do i = 0,maxitngp - 1
          gpnmax = max( gpnmax, lastgpns(i) )
      end do

      lastgpns(mod( iter, maxitngp )) = gpeucn2

      if ( gpeucn2 .ge. gpnmax ) then

          inform = 3

          if ( iprint .ge. 3 ) then
              write(*, 993) inform,maxitngp
              write(10,993) inform,maxitngp
          end if

          go to 500

      endif

C     Test whether the functional value is very small

      if ( f .le. fmin ) then

          inform = 4

          if ( iprint .ge. 3 ) then
              write(*, 994) inform,fmin
              write(10,994) inform,fmin
          end if

          go to 500

      end if

C     Test whether the number of iterations is exhausted

      if ( iter .ge. maxit ) then

          inform = 7

          if ( iprint .ge. 3 ) then
              write(*, 997) inform,maxit
              write(10,997) inform,maxit
          end if

          go to 500

      end if

C     Test whether the number of functional evaluations is exhausted

      if ( fcnt .ge. maxfc ) then

          inform = 8

          if ( iprint .ge. 3 ) then
              write(*, 998) inform,maxfc
              write(10,998) inform,maxfc
          end if

          go to 500

      end if

C     ==================================================================
C     The stopping criteria were not satisfied, a new iteration will be 
C     made
C     ==================================================================

      iter = iter + 1

C     ==================================================================
C     Save current values, f, x and g
C     ==================================================================

      fprev = f

      do i = 1,n
          s(i) = x(i)
          y(i) = g(i)
      end do

C     ==================================================================
C     Compute new iterate
C     ==================================================================

C     We abandon the current face if the norm of the internal gradient
C     (here, internal components of the continuous projected gradient)
C     is smaller than (1-eta) times the norm of the continuous 
C     projected gradient. Using eta=0.9 is a rather conservative 
C     strategy in the sense that internal iterations are preferred over 
C     SPG iterations. Replace eta = 0.9 by other tolerance in (0,1) if 
C     you find it convenient. 

      if ( gieucn2 .le. ometa2 * gpeucn2 ) then

C         ==============================================================
C         Some constraints should be abandoned. Compute the new iterate 
C         using an SPG iteration
C         ==============================================================

          ittype  = 'SPG'
          spgiter = spgiter + 1

C         Compute spectral steplength

          if ( iter .eq. 1 .or. sty .le. 0.0d0 ) then
              lamspg = max( 1.0d0, xnorm ) / sqrt( gpeucn2 )
          else
              lamspg = sts / sty
          end if
          lamspg = min( lspgma, max( lspgmi, lamspg ) )

C         Perform a line search with safeguarded quadratic interpolation 
C         along the direction of the spectral continuous projected 
C         gradient

          fcntprev = fcnt

          call spgls(n,x,m,lambda,rho,f,g,l,u,lamspg,nint,mininterp,
     +    fmin,maxfc,iprint,fcnt,inform,w(1),w(n+1),gamma,sigma1,sigma2,
     +    sterel,steabs,epsrel,epsabs,infrel,infabs) 

          spgfcnt = spgfcnt + ( fcnt - fcntprev ) 

          if ( inform .lt. 0 ) then

              if ( iprint .ge. 3 ) then
                  write(*, 1000) inform
                  write(10,1000) inform
              end if

              return
          end if

C         Compute the gradient at the new iterate

          if ( gtype .eq. 0 ) then
              call evalnal(n,x,m,lambda,rho,g,inform)
          else ! if ( gtype .eq. 1 ) then
              call evalnaldiff(n,x,m,lambda,rho,g,sterel,steabs,inform)
          end if
          gcnt = gcnt + 1
 
          if ( inform .lt. 0 ) then

              if ( iprint .ge. 3 ) then
                  write(*, 1000) inform
                  write(10,1000) inform
              end if

              return
          end if

      else

C         ==============================================================
C         The new iterate will belong to the closure of the current face
C         ==============================================================

          ittype = 'TN '
          tniter = tniter + 1

C         Compute trust-region radius

          if ( iter .eq. 1 ) then
              if( udelta0 .le. 0.0d0 ) then
                  delta = max( delmin, 0.1d0 * max( 1.0d0, xnorm ) )
              else
                  delta = udelta0
              end if
          else
              delta = max( delmin, 10.0d0 * sqrt( sts ) )
          end if

C         Shrink the point, its gradient and the bounds

          call shrink(nind,ind,n,x)
          call shrink(nind,ind,n,g)
          call shrink(nind,ind,n,l)
          call shrink(nind,ind,n,u)

C         Compute the descent direction solving the newtonian system by 
C         conjugate gradients

C         Set conjugate gradient stopping criteria. Default values are 
C         taken if you set ucgeps < 0 and ucgmaxit < 0, respectively. 
C         Otherwise, the parameters cgeps and cgmaxit will be the ones 
C         set by the user.

        call gp_ieee_signal2(
     +    cgmaxit, nind, nearlyq, ucgmaxit, cgscre,
     +    kappa, gpeucn2, gpeucn20, epsgpen2, epsgpsn,
     +    cgeps, acgeps, bcgeps, cgepsf, cgepsi, gpsupn, gpsupn0)

c          if( ucgmaxit .le. 0 ) then
c              if ( nearlyq ) then
c                  cgmaxit = nind
c              else
c                  if ( cgscre .eq. 1 ) then
c                      kappa = log10( gpeucn2 / gpeucn20 )/
c     +                        log10( epsgpen2 / gpeucn20 )
c                  else ! if ( cgscre .eq. 2 ) then
c                      kappa= log10( gpsupn / gpsupn0 ) / 
c     +                       log10( epsgpsn / gpsupn0 )
c                  end if
c                  kappa = max( 0.0d0, min( 1.0d0, kappa ) )
c                  cgmaxit = int(
c     +            ( 1.0d0 - kappa ) * max( 1.0d0, 10.0d0 * 
c     +            log10( dfloat( nind ) ) ) + kappa * dfloat( nind ) )
cc L. Martinez added to accelerate the iterations near the solution 
c                  cgmaxit = min(20,cgmaxit)
c              end if
cc              cgmaxit = 2 * nind
c          else
c              cgmaxit = ucgmaxit
c          end if
c
c          if ( cgscre .eq. 1 ) then
c              cgeps = sqrt( 10.0d0 ** ( acgeps * log10( gpeucn2 ) + 
c     +                bcgeps ) )
c          else ! if ( cgscre .eq. 2 ) then
c              cgeps = 10.0d0 ** ( acgeps * log10( gpsupn ) + bcgeps )
c          end if
c          cgeps = max( cgepsf, min( cgepsi, cgeps ) )

C         Call conjugate gradients

          call cg(nind,ind,n,x,m,lambda,rho,g,delta,l,u,cgeps,epsnqmp,
     +    maxitnqmp,cgmaxit,nearlyq,gtype,htvtype,trtype,iprint,ncomp,d,
     +    cgiter,rbdtype,rbdind,inform,w(1),w(n+1),w(2*n+1),w(3*n+1),
     +    w(4*n+1),theta,sterel,steabs,epsrel,epsabs,infrel,infabs)

          cgcnt = cgcnt + cgiter

          if ( inform .lt. 0 ) then

              if ( iprint .ge. 3 ) then
                  write(*, 1000) inform
                  write(10,1000) inform
              end if

              return

          end if

C         Compute maximum step

          if ( inform .eq. 2 ) then
              amax = 1.0d0
          else
              amax = infabs
              do i = 1,nind
                  if ( d(i) .gt. 0.0d0 ) then
                      amaxx = ( u(i) - x(i) ) / d(i)
                      if ( amaxx .lt. amax ) then
                          amax    = amaxx
                          rbdind  = i
                          rbdtype = 2
                      end if
                  else if ( d(i) .lt. 0.0d0 ) then
                      amaxx = ( l(i) - x(i) ) / d(i)
                      if ( amaxx .lt. amax ) then
                          amax    = amaxx
                          rbdind  = i
                          rbdtype = 1
                      end if
                  end if
               end do
          end if

C         Perform the line search

          tnintprev = tnintcnt
          tnexgprev = tnexgcnt
          tnexbprev = tnexbcnt

          fcntprev = fcnt

          call tnls(nind,ind,n,x,m,lambda,rho,l,u,f,g,d,amax,rbdtype,
     +    rbdind,nint,next,mininterp,maxextrap,fmin,maxfc,gtype,iprint,
     +    fcnt,gcnt,tnintcnt,tnexgcnt,tnexbcnt,inform,w(1),w(n+1),
     +    w(2*n+1),gamma,beta,sigma1,sigma2,sterel,steabs,epsrel,epsabs,
     +    infrel,infabs)

          if ( inform .lt. 0 ) then

              if ( iprint .ge. 3 ) then
                  write(*, 1000) inform
                  write(10,1000) inform
              end if

              return

          end if

          if ( tnintcnt .gt. tnintprev ) then
              tnintfe = tnintfe + ( fcnt - fcntprev )
          else if ( tnexgcnt .gt. tnexgprev ) then
              tnexgfe = tnexgfe + ( fcnt - fcntprev )
          else if ( tnexbcnt .gt. tnexbprev ) then
              tnexbfe = tnexbfe + ( fcnt - fcntprev )
          else
              tnstpcnt = tnstpcnt + 1
          end if

          tnfcnt = tnfcnt + ( fcnt - fcntprev )

C         Expand the point, its gradient and the bounds

          call expand(nind,ind,n,x)
          call expand(nind,ind,n,g)
          call expand(nind,ind,n,l)
          call expand(nind,ind,n,u)

C         If the line search (interpolation) in the Truncated Newton
C         direction stopped due to a very small step (inform = 6), we 
C         will discard this iteration and force a SPG iteration

C         Note that tnls subroutine was coded in such a way that in case
C         of inform = 6 termination the subroutine discards all what was 
C         done and returns with the same point it started

          if ( inform .eq. 6 ) then

              if ( iprint .ge. 3 ) then
                  write(*,*)  
                  write(*,*)  
     +            '     The previous TN iteration was discarded due to',
     +            '     a termination for very small step in the line ',
     +            '     search. A SPG iteration will be forced now.   '

                  write(10,*)  
                  write(10,*)  
     +            '     The previous TN iteration was discarded due to',
     +            '     a termination for very small step in the line ',
     +            '     search. A SPG iteration will be forced now.   '
              end if

              ittype  = 'SPG'
              spgiter = spgiter + 1

C             Compute spectral steplength

              if ( iter .eq. 1 .or. sty .le. 0.0d0 ) then
                  lamspg = max( 1.0d0, xnorm ) / sqrt( gpeucn2 )
              else
                  lamspg = sts / sty
              end if
              lamspg = min( lspgma, max( lspgmi, lamspg ) )

C             Perform a line search with safeguarded quadratic 
C             interpolation along the direction of the spectral 
C             continuous projected gradient

              fcntprev = fcnt

              call spgls(n,x,m,lambda,rho,f,g,l,u,lamspg,nint,mininterp,
     +        fmin,maxfc,iprint,fcnt,inform,w(1),w(n+1),gamma,sigma1,
     +        sigma2,sterel,steabs,epsrel,epsabs,infrel,infabs) 

              spgfcnt = spgfcnt + ( fcnt - fcntprev )

              if ( inform .lt. 0 ) then

                  if ( iprint .ge. 3 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return
              end if

C             Compute the gradient at the new iterate

              infotmp = inform

              if ( gtype .eq. 0 ) then
                  call evalnal(n,x,m,lambda,rho,g,inform)
              else ! if ( gtype .eq. 1 ) then
                  call evalnaldiff(n,x,m,lambda,rho,g,sterel,steabs,
     +            inform)
              end if
              gcnt = gcnt + 1
 
              if ( inform .lt. 0 ) then

                  if ( iprint .ge. 3 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return
              end if

              inform = infotmp

          end if

      end if

C     ==================================================================
C     Prepare for the next iteration 
C     ==================================================================

C     This adjustment/projection is ''por lo que las putas pudiera''

      do i = 1,n
          if ( x(i) .le. l(i) + max( epsrel * abs( l(i) ), epsabs ) ) 
     +    then
              x(i) = l(i)
          else if (x(i). ge. u(i) - max( epsrel * abs( u(i) ), epsabs )) 
     +    then  
              x(i) = u(i)
          end if
      end do

C     Compute x Euclidian norm

      xnorm = 0.0d0
      do i = 1,n
          xnorm = xnorm + x(i) ** 2
      end do
      xnorm = sqrt( xnorm )

C     Compute s = x_{k+1} - x_k, y = g_{k+1} - g_k, <s,s> and <s,y>

      sts = 0.0d0
      sty = 0.0d0
      do i = 1,n
          s(i) = x(i) - s(i)
          y(i) = g(i) - y(i)
          sts  = sts + s(i) ** 2
          sty  = sty + s(i) * y(i)
      end do

C     Compute continuous-project-gradient Euclidian and Sup norms,
C     internal gradient Euclidian norm, and store in nind the number of
C     free variables and in array ind their identifiers.

      nind    = 0
      gpsupn  = 0.0d0
      gpeucn2 = 0.0d0
      gieucn2 = 0.0d0
      do i = 1,n
          gpi     = min( u(i), max( l(i), x(i) - g(i) ) ) - x(i)
          gpsupn  = max( gpsupn, abs( gpi ) )
          gpeucn2 = gpeucn2 + gpi ** 2
          if ( x(i) .gt. l(i) .and. x(i) .lt. u(i) ) then
              gieucn2   = gieucn2 + gpi ** 2
              nind      = nind + 1
              ind(nind) = i
          end if
      end do

C     Print information of this iteration
c LM: progress bar for packmol
      call printbar(iprint, iter, maxit)

      if ( iprint .ge. 3 ) then 
          write(*, 983) iter,ittype
          write(*, 985) nprint,(x(i),i=1,nprint)
          write(*, 986) nprint,(g(i),i=1,nprint)
          write(*, 987) nprint,(min(u(i),max(l(i),x(i)-g(i)))-x(i),i=1,
     +    nprint)
          write(*, 988) min0(nprint,nind),nind,(ind(i),i=1,min0(nprint,
     +    nind))
          write(*, 1002) f,sqrt(gpeucn2),sqrt(gieucn2),gpsupn,nind,n,
     +    spgiter,tniter,fcnt,gcnt,cgcnt

          write(10,983) iter,ittype
          write(10,985) nprint,(x(i),i=1,nprint)
          write(10,986) nprint,(g(i),i=1,nprint)
          write(10,987) nprint,(min(u(i),max(l(i),x(i)-g(i)))-x(i),i=1,
     +    nprint)
          write(10,988) min0(nprint,nind),nind,(ind(i),i=1,min0(nprint,
     +    nind))
          write(10,1002) f,sqrt(gpeucn2),sqrt(gieucn2),gpsupn,nind,n,
     +    spgiter,tniter,fcnt,gcnt,cgcnt
      end if

C     ==================================================================
C     Test some stopping criteria that may occur inside the line 
C     searches 
C     ==================================================================

      if ( inform .eq. 6 ) then

          if ( iprint .ge. 3 ) then
              write(*, 996) inform,mininterp,epsrel,epsabs
              write(10,996) inform,mininterp,epsrel,epsabs
          end if

          go to 500

      end if

C     ==================================================================
C     Iterate 
C     ==================================================================

      go to 100

C     ==================================================================
C     End of main loop
C     ==================================================================

C     ==================================================================
C     Report output status and return
C     ==================================================================

 500  continue

C     Print final information

      if ( iprint .ge. 3 ) then
          write(*, 982) iter
          write(*, 985) nprint,(x(i),i=1,nprint)
          write(*, 986) nprint,(g(i),i=1,nprint)
          write(*, 987) nprint,(min(u(i),max(l(i),x(i)-g(i)))-x(i),i=1,
     +    nprint)
          write(*, 988) min0(nprint,nind),nind,(ind(i),i=1,min0(nprint,
     +    nind))
          write(*, 1002) f,sqrt(gpeucn2),sqrt(gieucn2),gpsupn,nind,n,
     +    spgiter,tniter,fcnt,gcnt,cgcnt

          write(10,982) iter
          write(10,985) nprint,(x(i),i=1,nprint)
          write(10,986) nprint,(g(i),i=1,nprint)
          write(10,987) nprint,(min(u(i),max(l(i),x(i)-g(i)))-x(i),i=1,
     +    nprint)
          write(10,988) min0(nprint,nind),nind,(ind(i),i=1,min0(nprint,
     +    nind))
          write(10,1002) f,sqrt(gpeucn2),sqrt(gieucn2),gpsupn,nind,n,
     +    spgiter,tniter,fcnt,gcnt,cgcnt
      end if

      return 

C     Non-executable statements

 977  format(/1X, 'Entry to GENCAN. Number of variables: ',I7)
 978  format(/1X,'Lower bounds (first ',I6, ' components): ',
     */,6(1X,1PD11.4))
 979  format(/1X,'Upper bounds (first ',I6, ' components): ',
     */,6(1X,1PD11.4))
 980  format(/1X,'Initial point (first ',I6, ' components): ',
     */,6(1X,1PD11.4))
 981  format(/1X,'GENCAN iteration: ',I6, ' (Initial point)')
 982  format(/1X,'GENCAN iteration: ',I6, ' (Final point)')
 983  format(/,1X,'GENCAN iteration: ',I6,
     *' (This point was obtained using a ',A3,' iteration)')
 985  format(1X,'Current point (first ',I6, ' components): ',
     */,6(1X,1PD11.4))
 986  format(1X,'Current gradient (first ',I6, ' components): ',
     */,6(1X,1PD11.4))
 987  format(1X,'Current continuous projected gradient (first ',I6, 
     *' components): ',/,6(1X,1PD11.4))
 988  format(1X,'Current free variables (first ',I6,
     *', total number ',I6,'): ',/,10(1X,I6))
 990  format(/1X,'Flag of GENCAN = ',I3,
     *' (convergence with Euclidian-norm of the projected gradient',
     */,1X,'smaller than ',1PD11.4,')')
 991  format(/1X,'Flag of GENCAN = ',I3,
     *' (convergence with sup-norm of the projected gradient',
     */,1X,'smaller than ',1PD11.4,')')
 992  format(/1X,'Flag of GENCAN= ',I3,
     *' (The algorithm stopped by lack of enough progress. This means',
     */,1X,'that  f(x_k) - f(x_{k+1}) .le. ',1PD11.4,
     *' * max [ f(x_j)-f(x_{j+1}, j < k ]',/,1X,'during ',I7,
     *' consecutive iterations')
 993  format(/1X,'Flag of GENCAN = ',I3,
     *' (The algorithm stopped because the order of the',
     */,1X,'Euclidian-norm of the continuous projected gradient did',
     *' not change during ',/,1X,I7,' consecutive iterations.',
     *' Probably, an exaggerated small norm of the',/,1X,'continuous',
     *' projected gradient is required for declaring convergence')
 994  format(/1X,'Flag of GENCAN = ',I3,
     *' (The algorithm stopped because the functional value is',
     */,1X,'smaller than ',1PD11.4)
 996  format(/1X,'Flag of GENCAN = ',I3,
     *' (Too small step in a line search. After having made at ',
     */,1X,'least ',I7,' interpolations, the steplength becames small.',
     *' Small means that',/,1X,'we were at point x with direction d',
     *' and took a step  alpha such that',/,1X,'alpha * |d_i| .lt.',
     *' max [',1PD11.4,' * |x_i|,',1PD11.4,' ] for all i)')
 997  format(/1X,'Flag of GENCAN = ',I3,
     *' (It was exceeded the maximum allowed number of iterations',
     */,1X,'(maxit=',I7,')')
 998  format(/1X,'Flag of GENCAN = ',I3,
     *' (It was exceeded the maximum allowed number of functional',
     */,1X,'evaluations (maxfc=',I7,')')
 1002 format(1X,'Functional value: ', 1PD11.4,
     */,1X,'Euclidian-norm of the continuous projected gradient: ',
     *1PD11.4,
     */,1X,'Euclidian-norm of the internal projection of gp: ',1PD11.4,
     */,1X,'Sup-norm of the continuous projected gradient: ',1PD11.4,
     */,1X,'Free variables at this point: ',I7,
     *' (over a total of ',I7,')',
     */,1X,'SPG iterations: ',I7,
     */,1X,'TN iterations: ',I7,
     */,1X,'Functional evaluations: ',I7,
     */,1X,'Gradient evaluations: ',I7,
     */,1X,'Conjugate gradient iterations: ',I7)
C1003 format(6X,I6,T22,D17.6,T43,D17.6)
C1003 format(6X,'Iter = ',I6,' f = ',1PD11.4,' gpsupn = ',1PD11.4)
 1000 format(/1X,'Flag of GENCAN = ',I3,' Fatal Error')

      end

C     ******************************************************************
C     ******************************************************************

      subroutine spgls(n,x,m,lambda,rho,f,g,l,u,lamspg,nint,mininterp,
     +fmin,maxfc,iprint,fcnt,inform,xtrial,d,gamma,sigma1,sigma2,sterel,
     +steabs,epsrel,epsabs,infrel,infabs) 

      implicit none

C     SCALAR ARGUMENTS
      integer fcnt,m,maxfc,mininterp,n,inform,iprint
      double precision epsabs,epsrel,f,fmin,gamma,infrel,infabs,lamspg,
     +        nint,sigma1,sigma2,steabs,sterel

C     ARRAY ARGUMENTS
      double precision d(n),g(n),l(n),lambda(m),rho(m),u(n),x(n),
     +        xtrial(n)
 
C     Safeguarded quadratic interpolation, used in the Spectral 
C     Projected Gradient directions.
C
C     On Entry
C
C     n        integer
C              the order of the x
C
C     x        double precision x(n)
C              current point
C
C     m        integer
C     lambda   double precision lambda(m)
C     rho      double precision rho(m)
C              These three parameters are not used nor modified by 
C              GENCAN and they are passed as arguments to the user-
C              defined subroutines evalal and evalnal to compute the 
C              objective function and its gradient, respectively. 
C              Clearly, in an Augmented Lagrangian context, if GENCAN is 
C              being used to solve the bound-constrainted subproblems, m 
C              would be the number of constraints, lambda the Lagrange 
C              multipliers approximation and rho the penalty parameters
C
C     f        double precision
C              function value at the current point
C
C     g        double precision g(n)
C              gradient vector at the current point
C
C     l        double precision l(n)
C              lower bounds
C
C     u        double precision u(n)
C              upper bounds
C
C     lamspg   double precision
C              spectral steplength
C
C     nint     double precision
C              constant for the interpolation. See the description of
C              sigma1 and sigma2 above. Sometimes we take as a new 
C              trial step the previous one divided by nint
C
C              RECOMMENDED: nint = 2.0
C
C     mininterp integer
C              constant for testing if, after having made at least 
C              mininterp interpolations, the steplength is so small. In 
C              that case failure of the line search is declared (may be 
C              the direction is not a descent direction due to an error 
C              in the gradient calculations) 
C
C              RECOMMENDED: mininterp = 4
C
C     fmin     double precision
C              functional value for the stopping criterion f <= fmin
C
C     maxfc    integer
C              maximum number of functional evaluations
C
C     iprint   integer
C              Commands printing. Nothing is printed if iprint is 
C              smaller than 2. If iprint is greater than or equal to 
C              2, GENCAN iterations information is printed. If iprint 
C              is greater than or equal to 3, line searches and 
C              Conjugate Gradients information is printed.
C
C              RECOMMENDED: iprint = 2
C
C              CONSTRAINTS: allowed values are just 2 or 3.
C
C     xtrial   double precision xtrial(n)
C     d        double precision d(n)
C              working vectors
C
C     gamma    double precision
C              constant for the Armijo criterion
C              f(x + alpha d) <= f(x) + gamma * alpha * <\nabla f(x),d>
C
C              RECOMMENDED: gamma = 10^{-4}
C
C     sigma1   double precision
C     sigma2   double precision
C              constant for the safeguarded interpolation
C              if alpha_new \notin [sigma1, sigma*alpha] then we take
C              alpha_new = alpha / nint
C
C              RECOMMENDED: sigma1 = 0.1 and sigma2 = 0.9
C
C     sterel   double precision
C     steabs   double precision
C              this constants mean a ``relative small number'' and ``an 
C              absolute small number'' for the increments in finite
C              difference approximations of derivatives
C
C              RECOMMENDED: epsrel = 10^{-7}, epsabs = 10^{-10} 
C
C     epsrel   double precision
C     epsabs   double precision
C     infrel   double precision
C     infabs   double precision
C              this constants mean a ``relative small number'', ``an 
C              absolute small number'', and ``infinite or a very big
C              number''. Basically, a quantity A is considered 
C              negligible with respect to another quantity B if |A| < 
C              max ( epsrel * |B|, epsabs ) 
C
C              RECOMMENDED: epsrel = 10^{-10}, epsabs = 10^{-20}, 
C                           infrel = 10^{+20}, infabs = 10^{+99}
C
C     On Return
C
C     x        double precision
C              final estimation of the solution
C
C     f        double precision
C              functional value at the final estimation 
C
C     fcnt     integer
C              number of functional evaluations used in the line search   
C
C     inform   integer
C              This output parameter tells what happened in this 
C              subroutine, according to the following conventions:
C
C              0 = convergence with an Armijo-like criterion
C                  (f(xnew) <= f(x) + gamma * alpha * <g,d>);
C
C              4 = the algorithm stopped because the functional value
C                  is smaller than fmin;
C
C              6 = too small step in the line search. After having made 
C                  at least mininterp interpolations, the steplength 
C                  becames small. ''small steplength'' means that we are 
C                  at point x with direction d and step alpha, and, for 
C                  all i, 
C
C                  | alpha * d(i) | <= max ( epsrel * |x(i)|, epsabs ). 
C 
C                  In that case failure of the line search is declared 
C                  (maybe the direction is not a descent direction due
C                  to an error in the gradient calculations). Use
C                  mininterp > maxfc to inhibit this criterion;
C
C              8 = it was achieved the maximum allowed number of
C                  function evaluations (maxfc);
C
C            < 0 = error in evalf subroutine.

C     LOCAL SCALARS
      logical samep
      integer i,interp
      double precision alpha,atmp,ftrial,gtd

C     Print presentation information

      if ( iprint .ge. 4 ) then
          write(*, 980) lamspg
          write(10,980) lamspg
      end if

C     Initialization

      interp = 0

C     Compute first trial point, spectral projected gradient direction, 
C     and directional derivative <g,d>.

      alpha = 1.0d0

      gtd = 0.0d0
      do i = 1,n
          xtrial(i) = min( u(i), max( l(i), x(i) - lamspg * g(i) ) )
          d(i)      = xtrial(i) - x(i)
          gtd       = gtd + g(i) * d(i)
      end do

      call evalal(n,xtrial,m,lambda,rho,ftrial,inform)
      fcnt = fcnt + 1

      if ( inform .lt. 0 ) then

          if ( iprint .ge. 4 ) then
              write(*, 1000) inform
              write(10,1000) inform
          end if

          return

      end if

C     Print information of the first trial

      if ( iprint .ge. 4 ) then
          write(*, 999) alpha,ftrial,fcnt
          write(10,999) alpha,ftrial,fcnt
      end if

C     Main loop

 100  continue

C     Test Armijo stopping criterion

      if ( ftrial .le. f + gamma * alpha * gtd ) then

          f = ftrial

          do i = 1,n
              x(i) = xtrial(i)
          end do

          inform = 0

          if ( iprint .ge. 4 ) then
              write(*, 990) inform
              write(10,990) inform
          end if

          go to 500

      end if

C     Test whether f is very small

      if ( ftrial .le. fmin ) then

          f = ftrial

          do i = 1,n
              x(i) = xtrial(i)
          end do

          inform = 4

          if ( iprint .ge. 4 ) then
              write(*, 994) inform
              write(10,994) inform
          end if

          go to 500

      end if

C     Test whether the number of functional evaluations is exhausted

      if ( fcnt .ge. maxfc ) then

          if ( ftrial .lt. f ) then

              f = ftrial

              do i = 1,n
                  x(i) = xtrial(i)
              end do

          end if

          inform = 8

          if ( iprint .ge. 4 ) then
              write(*, 998) inform
              write(10,998) inform
          end if

          go to 500

      end if

C     Compute new step (safeguarded quadratic interpolation)

      interp = interp + 1

      if ( alpha .lt. sigma1 ) then
          alpha = alpha / nint      

      else
          atmp = ( - gtd * alpha ** 2 ) / 
     +           ( 2.0d0 * ( ftrial - f - alpha * gtd ) )

          if ( atmp .lt. sigma1 .or. atmp .gt. sigma2 * alpha ) then
              alpha = alpha / nint

          else
              alpha = atmp
          end if
      end if

C     Compute new trial point

      do i = 1,n
          xtrial(i) = x(i) + alpha * d(i)
      end do

      call evalal(n,xtrial,m,lambda,rho,ftrial,inform)
      fcnt = fcnt + 1

      if ( inform .lt. 0 ) then

          if ( iprint .ge. 4 ) then
              write(*, 1000) inform
              write(10,1000) inform
          end if

          return

      end if

C     Print information of the current trial

      if ( iprint .ge. 4 ) then
          write(*, 999) alpha,ftrial,fcnt
          write(10,999) alpha,ftrial,fcnt
      end if

C     Test whether at least mininterp interpolations were made and two 
C     consecutive iterates are close enough

      samep = .true.
      do i = 1,n
         if ( abs( alpha * d(i) ) .gt. 
     +        max( epsrel * abs( x(i) ), epsabs ) ) then
             samep = .false.
         end if
      end do

      if ( interp .ge. mininterp .and. samep ) then

          if ( ftrial .lt. f ) then

              f = ftrial

              do i = 1,n
                  x(i) = xtrial(i)
              end do

          end if

          inform = 6

          if ( iprint .ge. 4 ) then
              write(*, 996) inform
              write(10,996) inform
          end if
  
          go to 500

      end if

C     Iterate

      go to 100

C     Return

 500  continue

      return

C     Non-executable statements

 980  format(/,6x,'SPG (spectral steplength ',1PD11.4,')',/,/,
     *         6x,'SPG Line search')
 999  format(6x,'Alpha= ',1PD11.4,' F= ',1PD11.4,' FE= ',I5)
 990  format(6x,'Flag of SPG Line search = ',I3,
     *          ' (Convergence with an Armijo-like criterion)')
 994  format(6x,'Flag of SPG Line search = ',I3,
     *          ' (Small functional value, smaller than ',/,
     *       6X,'parameter fmin)')
 996  format(6x,'Flag of SPG Line search = ',I3,
     *          ' (Too small step in the interpolation)')
 998  format(6x,'Flag of SPG Line search = ',I3,
     *          ' (Too many functional evaluations)')
 1000 format(6x,'Flag of SPG Line search = ',I3,' Fatal Error')

      end

C     ******************************************************************
C     ******************************************************************

      subroutine cg(nind,ind,n,x,m,lambda,rho,g,delta,l,u,eps,epsnqmp,
     +maxitnqmp,maxit,nearlyq,gtype,htvtype,trtype,iprint,ncomp,s,iter,
     +rbdtype,rbdind,inform,w,y,r,d,sprev,theta,sterel,steabs,epsrel,
     +epsabs,infrel,infabs)

      implicit none

C     SCALAR ARGUMENTS
      logical nearlyq
      integer gtype,htvtype,inform,iprint,iter,m,maxit,maxitnqmp,n,
     +        ncomp,nind,trtype,rbdind,rbdtype
      double precision delta,eps,epsnqmp,epsabs,epsrel,infrel,infabs,
     +        steabs,sterel,theta

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision d(n),g(n),l(n),lambda(m),r(n),rho(m),s(n),
     +        sprev(n),u(n),w(n),x(n),y(n)

C     This subroutine implements the Conjugate Gradients method for 
C     minimizing the quadratic approximation q(s) of f(x) at x, where
C
C     q(s) = 1/2 s^T H s + g^T s,
C
C        H = \nabla^2 f(x),
C
C        g = \nabla f(x),
C
C     subject to || s || <= delta and l <= x + s <= u.
C
C     In the constraint ''|| s || <= delta'', the norm will be the
C     Euclidian norm if the input parameter trtype is equal to 0, and
C     it will be the Sup norm if trtype is equal to 1.
C
C     The method returns an approximation s to the solution such that 
C     ||H s + g||_2 <= eps * ||g||_2; or converges to the boundary of 
C     ||s||_2 <= delta and l <= x + s <= u; or finds a point s and a 
C     direction d such that q(s + alpha d) = q(s) for any alpha, i.e., 
C     d^T H d = g^T d = 0.
C
C     On Entry
C
C     nind     integer
C              number of free variables (this is thee dimension in 
C              which this subroutine will work)
C
C     ind      integer ind(n)
C              array which contains, in the first nind positions, the
C              identifiers of the free variables
C
C     n        integer
C              dimension of the full space
C
C     x        double precision x(n)
C              point at which f function is being approximated by the
C              quadratic model
C
C              The first nind positions of x contains the free variables 
C              x_ind(1), x_ind(2), ..., x_ind(nind).
C
C     m        integer
C     lambda   double precision lambda(m)
C     rho      double precision rho(m)
C              These three parameters are not used nor modified by 
C              GENCAN and they are passed as arguments to the user-
C              defined subroutines evalal and evalnal to compute the 
C              objective function and its gradient, respectively. 
C              Clearly, in an Augmented Lagrangian context, if GENCAN is 
C              being used to solve the bound-constrainted subproblems, m 
C              would be the number of constraints, lambda the Lagrange 
C              multipliers approximation and rho the penalty parameters
C
C     g        double precision g(n)
C              linear coefficient of the quadratic function
C
C              This is \nabla f(x) and it also contains in the first 
C              nind positions the components g_ind(1), g_ind(2), ..., 
C              g_ind(nind).
C    
C              IMPORTANT: the linear algebra of this subroutine lies in 
C              a space of dimension nind. The value of the full 
C              dimension n, the non-free variables (which are at the end 
C              of array x) and its gradient components (which are at the 
C              and of array g) are, at this moment, being used to 
C              approximate the Hessian times vector products by 
C              incremental quotients.
C
C     delta    double precision
C              trust region radius (||s||_2 <= delta) 
C    
C     l        double precision l(n)
C              lower bounds on x + s. It components are ordered in the 
C              same way as x and g.
C
C     u        double precision u(n)
C              upper bounds on x + s. It components are ordered in the 
C              same way as x, g and l.
C
C     eps      double precision
C              tolerance for the stopping criterion ||H s + g||_2 < eps 
C              * ||g||_2
C
C     epsnqmp  double precision
C              See below
C
C     maxitnqmp integer
C              This and the previous one parameter are used for a 
C              stopping criterion of the conjugate gradient 
C              subalgorithm. If the progress in the quadratic model is 
C              less or equal than a fraction of the best progress 
C              ( epsnqmp * bestprog ) during maxitnqmp consecutive 
C              iterations then CG is stopped by not enough progress of 
C              the quadratic model.
C
C              RECOMMENDED: epsnqmp = 1.0d-4, maxitnqmp = 5
C
C     maxit    integer
C              maximum number of iterations allowed
C
C     nearlyq  logical
C              if function f is (nearly) quadratic, use the option 
C              nearlyq = TRUE. Otherwise, keep the default option.
C
C              if, in an iteration of CG we find a direction d such that
C              d^T H d <= 0 then we take the following decision:
C
C              (i) if nearlyq = TRUE then take direction d and try to go 
C              to the boundary choosing the best point among the two 
C              point at the boundary and the current point. 
C
C              (ii) if nearlyq = FALSE then we stop at the current 
C              point.
C
C              RECOMMENDED: nearlyq = FALSE
C
C     gtype    integer
C              type of gradient calculation
C              gtype = 0 means user suplied evalg subroutine,
C              gtype = 1 means central difference approximation.
C
C              RECOMMENDED: gtype = 0
C
C              (provided you have the evalg subroutine)
C
C     htvtype  integer
C              type of Hessian times vector product calculation
C              htvtype = 0 means user supplied evalhd subroutine,
C              htvtype = 1 means incremental quotients approximation.
C
C              RECOMMENDED: htvtype = 1
C
C              (you take some risk using this option but, unless you 
C              have a good evalhd subroutine, incremental quotients is a
C              very cheap option)
C
C     trtype   integer
C              type of trust-region radius
C              trtype = 0 means 2-norm trust-region
C              trtype = 1 means infinite-norm trust-region
C
C              RECOMMENDED: trtype = 0
C
C     iprint   integer
C              Commands printing. Nothing is printed if iprint is 
C              smaller than 2. If iprint is greater than or equal to 
C              2, GENCAN iterations information is printed. If iprint 
C              is greater than or equal to 3, line searches and 
C              Conjugate Gradients information is printed.
C
C              RECOMMENDED: iprint = 2
C
C              CONSTRAINTS: allowed values are just 2 or 3.
C
C     ncomp    integer
C              This constant is just for printing. In a detailed 
C              printing option, ncomp component of some vectors will be 
C              printed
C
C              RECOMMENDED: ncomp = 5
C
C              CONSTRAINTS: ncomp >= 0
C
C     w        double precision w(n)
C     y        double precision y(n)
C     r        double precision r(n)
C     d        double precision d(n)
C     sprev    double precision sprev(n)
C              working vectors
C
C     theta    double precision
C              constant for the angle condition, i.e., at iteration k we 
C              need a direction d_k such that <gk,dk> <= - theta 
C              ||gk||_2 ||dk||_2, where gk is \nabla f(xk)
C
C              RECOMMENDED: theta = 10^{-6}
C
C     sterel   double precision
C     steabs   double precision
C              this constants mean a ``relative small number'' and ``an 
C              absolute small number'' for the increments in finite
C              difference approximations of derivatives
C
C              RECOMMENDED: epsrel = 10^{-7}, epsabs = 10^{-10} 
C
C     epsrel   double precision
C     epsabs   double precision
C     infrel   double precision
C     infabs   double precision 
C              this constants mean a ``relative small number'', ``an 
C              absolute small number'', and ``infinite or a very big 
C              number''. Basically, a quantity A is considered 
C              negligible with respect to another quantity B if |A| < 
C              max ( epsrel * |B|, epsabs ) 
C
C              RECOMMENDED: epsrel = 10^{-10}, epsabs = 10^{-20},
C                           infrel = 10^{+20}, infabs = 10^{+99}
C
C     On Return
C
C     s        double precision s(n)
C              final estimation of the solution
C
C     iter     integer
C              number of Conjugate Gradient iterations performed
C
C     inform   integer
C              termination parameter:
C
C              0 = convergence with ||H s + g||_2 <= eps * ||g||_2;
C
C              1 = convergence to the boundary of ||s||_2 <= delta;
C
C              2 = convergence to the boundary of l - x <= s <= u - x;
C
C              3 = stopping with s = sk  such that <gk,sk> <= -t heta 
C                  ||gk||_2 ||sk||_2 and <gk,s_{k+1}> > - theta 
C                  ||gk||_2 ||s_{k+1}||_2;
C
C              4 = not enough progress of the quadratic model during
C                  maxitnqmp iterations, i.e., during maxitnqmp 
C                  iterations | q - qprev | <= max ( epsrel * | q |, 
C                  epsabs );
C
C              6 = very similar consecutive iterates, for two 
C                  consecutive iterates x and y, for all i | x(i) - 
C                  y(i) | <= max ( epsrel * | x(i) |, epsabs );
C
C              7 = stopping with d such that d^T H d = 0 and g^T d = 0;
C
C              8 = too many iterations;
C
C            < 0 = error in evalhd subroutine.

C     LOCAL SCALARS
      character * 5 rbdtypea
      logical samep
      integer i,itnqmp,rbdnegaind,rbdnegatype,rbdposaind,rbdposatype
      double precision aa,alpha,amax,amax1,amax1n,amaxn,amax2,amax2n,
     +        amax2nx,amax2x,bb,bestprog,beta,cc,currprog,dd,dnorm2,dtr,
     +        dts,dtw,gnorm2,gts,norm2s,q,qamax,qamaxn,qprev,rnorm2,
     +        rnorm2prev,snorm2,snorm2prev

C     ==================================================================
C     Initialization
C     ==================================================================

      gnorm2   = norm2s(nind,g)

      iter     =      0
      itnqmp   =      0
      qprev    = infabs
      bestprog =  0.0d0

      do i = 1,nind
          s(i) = 0.0d0
          r(i) =  g(i)
      end do

      q        =  0.0d0
      gts      =  0.0d0
      snorm2   =  0.0d0
      rnorm2   = gnorm2

C     ==================================================================
C     Print initial information
C     ==================================================================

      if ( iprint .ge. 4 ) then
          write(*, 980) maxit,eps
          if ( trtype .eq. 0 ) then
              write(*, 981) delta
          else if ( trtype .eq. 1 ) then
              write(*, 982) delta
          else
              write(*, 983)
          end if
          write(*, 984) iter,rnorm2,sqrt(snorm2),q

          write(10,980) maxit,eps
          if ( trtype .eq. 0 ) then
              write(10,981) delta
          else if ( trtype .eq. 1 ) then
              write(10,982) delta
          else
              write(10,983)
          end if
          write(10,984) iter,rnorm2,sqrt(snorm2),q

      end if

C     ==================================================================
C     Main loop
C     ==================================================================

 100  continue

C     ==================================================================
C     Test stopping criteria
C     ==================================================================

C     if ||r||_2 = ||H s + g||_2 <= eps * ||g||_2 then stop

      if ( rnorm2 .le. 1.0d-16 .or.
     +     ( ( rnorm2 .le. eps ** 2 * gnorm2 .or.
     +       ( rnorm2 .le. 1.0d-10 .and. iter .ne. 0 ) ) 
     +       .and. iter .ge. 4 ) ) then

          inform = 0

          if ( iprint .ge. 4 ) then
              write(*, 990) inform
              write(10,990) inform
          end if
  
          go to 500

      end if

C     if the maximum number of iterations was achieved then stop

      if ( iter .ge. max(4, maxit) ) then

          inform = 8

          if ( iprint .ge. 4 ) then
              write(*, 998) inform
              write(10,998) inform
          end if
  
          go to 500

      end if

C     ==================================================================
C     Compute direction
C     ==================================================================

      if ( iter .eq. 0 ) then

          do i = 1,nind
              d(i) = - r(i)
          end do

          dnorm2 =   rnorm2
          dtr    = - rnorm2

      else

          beta = rnorm2 / rnorm2prev

          do i = 1,nind
              d(i) = - r(i) + beta * d(i)
          end do

          dnorm2 = rnorm2 - 2.0d0 * beta * ( dtr + alpha * dtw ) + 
     +             beta ** 2 * dnorm2
          dtr    = - rnorm2 + beta * ( dtr + alpha * dtw )

      end if

C     Force d to be a descent direction of q(s), i.e.,
C     <\nabla q(s), d> = <H s + g, d> = <r, d> \le 0.

      if ( dtr .gt. 0.0d0 ) then

          do i = 1,nind
              d(i) = - d(i)
          end do
          dtr = - dtr

      end if

C     ==================================================================
C     Compute d^T H d
C     ==================================================================

C     w = A d

      if ( htvtype .eq. 0 ) then
          call calchd(nind,ind,x,d,g,n,x,m,lambda,rho,w,y,sterel,steabs,
     +    inform)

      else if ( htvtype .eq. 1 ) then
          call calchddiff(nind,ind,x,d,g,n,x,m,lambda,rho,gtype,w,y,
     +    sterel,steabs,inform)
      end if

      if ( inform .lt. 0 ) then

          if ( iprint .ge. 4 ) then
              write(*, 1000) inform
              write(10,1000) inform
          end if

          return
      end if

C     Compute d^T w and ||w||^2

      dtw = 0.0d0
      do i = 1,nind
          dtw = dtw + d(i) * w(i)
      end do 

C     ==================================================================
C     Compute maximum step
C     ==================================================================

C     amax1 > 0 and amax1n < 0 are the values of alpha such that 
C     ||s + alpha * d||_2 or ||s + alpha * d||_\infty = delta 

      dts = 0.0d0
      do i = 1,nind
          dts = dts + d(i) * s(i)
      end do

C     Euclidian-norm trust radius

      if ( trtype .eq. 0 ) then

          aa = dnorm2
          bb = 2.0d0 * dts
          cc = snorm2 - delta ** 2
          dd = sqrt( bb ** 2 - 4.0d0 * aa * cc )

          amax1  = ( - bb + dd ) / ( 2.0d0 * aa )
          amax1n = ( - bb - dd ) / ( 2.0d0 * aa )

C     Sup-norm trust radius

      else if ( trtype .eq. 1 ) then

          amax1  =  infabs
          amax1n = -infabs

          do i = 1,nind
              if ( d(i) .gt. 0.0d0 ) then
                  amax1  = min( amax1,  (   delta - s(i) ) / d(i) )
                  amax1n = max( amax1n, ( - delta - s(i) ) / d(i) )
              else if ( d(i) .lt. 0.0d0 ) then
                  amax1  = min( amax1,  ( - delta - s(i) ) / d(i) )
                  amax1n = max( amax1n, (   delta - s(i) ) / d(i) )
              end if
          end do

      end if

C     amax2 > 0 and amax2n < 0 are the maximum and the minimum values of 
C     alpha such that l - x <= s + alpha * d <= u - x, respectively

      amax2  =   infabs
      amax2n = - infabs

      do i = 1,nind
          if ( d(i) .gt. 0.0d0 ) then
C             if (u(i).lt.infrel) then
                  amax2x = ( u(i) - x(i) - s(i) ) / d(i)
                  if ( amax2x .lt. amax2 ) then
                      amax2       = amax2x
                      rbdposaind  = i
                      rbdposatype = 2
                  end if
C             end if
C             if (l(i).gt.-infrel) then    
                  amax2nx = ( l(i) - x(i) - s(i) ) / d(i)
                  if ( amax2nx .gt. amax2n ) then
                      amax2n      = amax2nx
                      rbdnegaind  = i
                      rbdnegatype = 1
                  end if
C             end if
          else if ( d(i) .lt. 0.0d0 ) then
C             if (l(i).gt.-infrel) then    
                  amax2x = ( l(i) - x(i) - s(i) ) / d(i)
                  if ( amax2x .lt. amax2 ) then
                      amax2       = amax2x
                      rbdposaind  = i
                      rbdposatype = 1
                  end if
C             end if
C             if (u(i).lt.infrel) then
                  amax2nx = ( u(i) - x(i) - s(i) ) / d(i)
                  if ( amax2nx .gt. amax2n ) then
                      amax2n      = amax2nx
                      rbdnegaind  = i
                      rbdnegatype = 2
                  end if
C             end if
          end if
      end do

C     Compute amax as the minimum among amax1 and amax2, and amaxn as 
C     the minimum among amax1n and amax2n. Moreover change amaxn by 
C     - amaxn to have amax and amaxn as maximum steps along d direction 
C     (and not -d in the case of amaxn)

      amax  = min( amax1 , amax2  )
      amaxn = max( amax1n, amax2n )

C     ==================================================================
C     Compute the step (and the quadratic functional value at the new 
C     point)
C     ==================================================================

      qprev = q

C     If d^T H d > 0 then take the conjugate gradients step

      if ( dtw .gt. 0.0d0 ) then

          alpha = min( amax, rnorm2 / dtw )

          q = q + 0.5d0 * alpha ** 2 * dtw + alpha * dtr

C     If d^T H d <= 0 and function f is nearly quadratic then take the 
C     point with the minimum functional value (q) among the current one 
C     and the ones which are at the boundary, i.e., the best one between 
C     q(s), q(s + amax*d) and q(s + amaxn*d).

      else

          qamax = q + 0.5d0 * amax ** 2 * dtw + amax * dtr

C         If we are at iteration zero then take the maximum positive
C         step in the minus gradient direction

          if ( iter .eq. 0 ) then

              alpha = amax
              q     = qamax

C         If we are not in the first iteration then if function f is 
C         nearly quadratic and q(s + amax * d) or q(s + amaxn * d) is 
C         smaller than q(s), go to the best point in the boundary

          else 

              qamaxn = q + 0.5d0 * amaxn ** 2 * dtw + amaxn * dtr

              if ( nearlyq .and. 
     +             ( qamax .lt. q .or. qamaxn .lt. q ) ) then

                  if ( qamax .lt. qamaxn ) then
                      alpha = amax
                      q     = qamax
                  else
                      alpha = amaxn
                      q     = qamaxn
                  end if

C         Else, stop at the current point

              else

                  inform = 7

                  if ( iprint .ge. 4 ) then
                      write(*, 997) inform
                      write(10,997) inform
                  end if
  
                  go to 500

              end if

          end if
      end if

C     ==================================================================
C     Compute new s
C     ==================================================================

      do i = 1,nind
          sprev(i) = s(i)
          s(i)     = s(i) + alpha * d(i)
      end do

      snorm2prev = snorm2
      snorm2     = snorm2 + alpha ** 2 * dnorm2 + 2.0d0 * alpha * dts

C     ==================================================================
C     Compute the residual r = H s + g
C     ==================================================================

      rnorm2prev = rnorm2

      do i = 1,nind
          r(i) = r(i) + alpha * w(i)
      end do

      rnorm2 = norm2s(nind,r)

C     ==================================================================
C     Increment number of iterations
C     ==================================================================

      iter = iter + 1

C     ==================================================================
C     Print information of this iteration
C     ==================================================================

      if ( iprint .ge. 4 ) then
          write(*, 984) iter,sqrt(rnorm2),sqrt(snorm2),q
          write(10,984) iter,sqrt(rnorm2),sqrt(snorm2),q
      end if

C     ==================================================================
C     Test other stopping criteria
C     ==================================================================

C     Test angle condition

      gts = 0.0d0
      do i = 1,nind
          gts = gts + g(i) * s(i)
      end do

      if ( gts .gt. 0.0d0 .or. 
     +     gts ** 2 .lt. theta ** 2 * gnorm2 * snorm2 ) then

          do i = 1,nind
              s(i) = sprev(i)
          end do

          snorm2 = snorm2prev

          q = qprev

          inform = 3

          if ( iprint .ge. 4 ) then
              write(*, 993) inform
              write(10,993) inform
          end if

          go to 500

      end if

C     If we are in the boundary of the box also stop

      if ( alpha .eq. amax2 .or. alpha .eq. amax2n ) then

          if ( alpha .eq. amax2 ) then
              rbdind  = rbdposaind
              rbdtype = rbdposatype
          else ! if (alpha.eq.amax2n) then
              rbdind  = rbdnegaind
              rbdtype = rbdnegatype
          end if

          if ( rbdtype .eq. 1 ) then
              rbdtypea = 'lower'
          else ! if (rbdtype.eq.2) then
              rbdtypea = 'upper'
          end if
 
          inform = 2

          if ( iprint .ge. 4 ) then
              write(*, 992) inform,ind(rbdind),rbdtypea
              write(10,992) inform,ind(rbdind),rbdtypea
          end if
  
          go to 500

      end if

C     If we are in the boundary of the trust region then stop

      if ( alpha .eq. amax1 .or. alpha .eq. amax1n ) then

          inform = 1

          if ( iprint .ge. 4 ) then
              write(*, 991) inform
              write(10,991) inform
          end if
  
          go to 500

      end if

C     If two consecutive iterates are much close then stop

      samep = .true.
      do i = 1,nind
         if ( abs( alpha * d(i) ) .gt. 
     +        max( epsrel * abs( s(i) ), epsabs ) ) then
              samep = .false.
          end if
      end do

      if ( samep ) then

          inform = 6

          if ( iprint .ge. 4 ) then
              write(*, 996) inform
              write(10,996) inform
          end if
  
          go to 500

      end if

C     Test whether we performed many iterations without good progress of
C     the quadratic model

C     if (abs( q - qprev ) .le. max( epsrel * abs( qprev ), epsabs ) ) 
C    +then

C         itnqmp = itnqmp + 1

C         if ( itnqmp .ge. maxitnqmp ) then

C             inform = 4

C             if ( iprint .ge. 4 ) then
C                 write(*,994)  inform,itnqmp
C                 write(10,994) inform,itnqmp
C             end if
  
C             go to 500

C         endif

C     else
C         itnqmp= 0
C     endif

C     Test whether we performed many iterations without good progress of
C     the quadratic model 

      currprog = qprev - q
      bestprog = max( currprog, bestprog )

      if ( currprog .le. epsnqmp * bestprog ) then

          itnqmp = itnqmp + 1

          if ( itnqmp .ge. maxitnqmp ) then
              inform = 4

              if ( iprint .ge. 4 ) then
                  write(*, 994) inform,itnqmp,epsnqmp,bestprog
                  write(10,994) inform,itnqmp,epsnqmp,bestprog
              end if

              go to 500
          endif

      else
          itnqmp = 0
      endif

C     ==================================================================
C     Iterate
C     ==================================================================

      go to 100

C     ==================================================================
C     End of main loop
C     ==================================================================

C     ==================================================================
C     Return
C     ==================================================================

 500  continue

C     Print final information

      if ( iprint .ge. 4 ) then
          write(*, 985) min0(nind,ncomp),(s(i),i=1,min0(nind,ncomp))
          write(10,985) min0(nind,ncomp),(s(i),i=1,min0(nind,ncomp))
      end if

      return

C     Non-executable statements

 980  format(/,6x,'Conjugate gradients (maxit= ',I7,' acc= ',1PD11.4,
     *')')
 981  format(6x,'Using Euclidian trust region (delta= ',1PD11.4,
     *')')
 982  format(6x,'Using sup-norm trust region (delta= ',1PD11.4,')')
 983  format(6x,'Unknown trust-region type')
 984  format(6x,'CG iter= ',I5,' rnorm: ',1PD11.4,' snorm= ',1PD11.4,
     *' q= ',1PD11.4)
 985  format(/,6x,'Truncated Newton direction (first ',I6, 
     *' components): ',/,1(6x,6(1PD11.4,1x)))
 990  format(6x,'Flag of CG = ',I3,' (Convergence with small residual)')
 991  format(6x,'Flag of CG = ',I3,
     *' (Convergence to the trust region boundary)')
 992  format(6x,'Flag of CG = ',I3,
     *' (Convergence to the boundary of the box constraints,',/,6x,
     *'taking step >= 1, variable ',I6,' will reaches its ',A5,
     *' bound)')
 993  format(6x,'Flag of CG = ',I3,
     *' (The next CG iterate will not satisfy the angle condition)')
 994  format(6x,'Flag of CG = ',I3,
     *' (Not enough progress in the quadratic model. This means',/,6x,
     *'that the progress of the last ',I7,' iterations was smaller ', 
     *'than ',/,6x,1PD11.4,' times the best progress (',1PD11.4,')')
 996  format(6x,'Flag of CG = ',I3,
     *' (Very near consecutive iterates)')
 997  format(6x,'Flag of CG= ',I3,
     *' (d such that d^T H d = 0 and g^T d = 0 was found)')
 998  format(6x,'Flag of CG = ',I3,' (Too many GC iterations)')
 1000 format(6x,'Flag of CG = ',I3,' Fatal Error')

      end

C     *****************************************************************
C     *****************************************************************
      subroutine tnls(nind,ind,n,x,m,lambda,rho,l,u,f,g,d,amax,rbdtype,
     +rbdind,nint,next,mininterp,maxextrap,fmin,maxfc,gtype,iprint,fcnt,
     +gcnt,intcnt,exgcnt,exbcnt,inform,xplus,xtmp,xbext,gamma,beta,
     +sigma1,sigma2,sterel,steabs,epsrel,epsabs,infrel,infabs)

      implicit none

C     SCALAR ARGUMENTS
      integer exbcnt,exgcnt,fcnt,gcnt,gtype,inform,intcnt,iprint,m,
     +        maxextrap,maxfc,mininterp,n,nind,rbdind,rbdtype
      double precision amax,beta,epsabs,epsrel,f,fmin,gamma,infabs,
     +        infrel,next,nint,sigma1,sigma2,steabs,sterel

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision d(n),g(n),l(n),lambda(m),rho(m),u(n),x(n),
     +        xbext(n),xplus(n),xtmp(n)

C     This subroutine implements the line search used in the Truncated
C     Newton direction.
C
C     On Entry
C
C     nind     integer
C              number of free variables (this is thee dimension in 
C              which this subroutine will work)
C
C     ind      integer ind(n)
C              array which contains, in the first nind positions, the
C              identifiers of the free variables
C
C     n        integer
C              dimension of the full space
C
C     x        double precision x(n)
C              current point
C
C              The first nind positions of x contains the free variables 
C              x_ind(1), x_ind(2), ..., x_ind(nind).
C
C     m        integer
C     lambda   double precision lambda(m)
C     rho      double precision rho(m)
C              These three parameters are not used nor modified by 
C              GENCAN and they are passed as arguments to the user-
C              defined subroutines evalal and evalnal to compute the 
C              objective function and its gradient, respectively. 
C              Clearly, in an Augmented Lagrangian context, if GENCAN is 
C              being used to solve the bound-constrainted subproblems, m 
C              would be the number of constraints, lambda the Lagrange 
C              multipliers approximation and rho the penalty parameters
C
C     l        double precision l(nind)
C              lower bounds on x. It components are ordered in the
C              same way as x and g.
C
C     u        double precision u(nind)
C              upper bounds on x. It components are ordered in the
C              same way as x, g and l.
C
C     f        double precision
C              functional value at x
C
C     g        double precision g(n)
C              gradient vector at x
C
C              It also contains in the first nind positions the 
C              components g_ind(1), g_ind(2), ..., g_ind(nind).
C    
C              IMPORTANT: the linear algebra of this subroutine lies in 
C              a space of dimension nind. The value of the full 
C              dimension n, the non-free variables (which are at the end 
C              of array x) and its gradient components (which are at the 
C              end of array g) are also used and updated any time the 
C              gradient is being computed.
C
C     d        double precision d(nind)
C              descent direction 
C    
C     amax     double precision
C
C     rbdtype  integer
C
C     rbdind   integer
C
C     nint     double precision
C              constant for the interpolation. See the description of
C              sigma1 and sigma2 above. Sometimes we take as a new 
C              trial step the previous one divided by nint
C
C              RECOMMENDED: nint = 2.0
C
C     next     double precision
C              constant for the extrapolation
C              when extrapolating we try alpha_new = alpha * next
C
C              RECOMMENDED: next = 2.0
C
C     mininterp integer
C              constant for testing if, after having made at least 
C              mininterp interpolations, the steplength is so small. 
C              In that case failure of the line search is declared (may 
C              be the direction is not a descent direction due to an 
C              error in the gradient calculations) 
C
C              RECOMMENDED: mininterp = 4
C
C     maxextrap integer
C              constant to limit the number of extrapolations
C
C              RECOMMENDED: maxextrap = 1000 (a big number)
C
C     fmin     double precision
C              functional value for the stopping criteria f <= fmin
C
C     maxfc    integer
C              maximum number of functional evaluations
C
C     gtype    integer
C              type of gradient calculation
C              gtype = 0 means user suplied evalg subroutine,
C              gtype = 1 means central difference approximation.
C
C              RECOMMENDED: gtype = 0
C
C              (provided you have the evalg subroutine)
C
C     iprint   integer
C              Commands printing. Nothing is printed if iprint is 
C              smaller than 2. If iprint is greater than or equal to 
C              2, GENCAN iterations information is printed. If iprint 
C              is greater than or equal to 3, line searches and 
C              Conjugate Gradients information is printed.
C
C              RECOMMENDED: iprint = 2
C
C              CONSTRAINTS: allowed values are just 2 or 3.
C
C     xplus    double precision xplus(nind)
C     xtmp     double precision xtmp(nind)
C     xbext    double precision xbext(nind) 
C              working vectors
C
C     gamma    double precision
C              constant for the Armijo criterion
C              f(x + alpha d) <= f(x) + gamma * alpha * <\nabla f(x),d>
C
C              RECOMMENDED: gamma = 10^{-4}
C
C     beta     double precision
C              constant for the beta condition <dk, g(xk + dk)>  <  beta 
C              * <dk,gk>. If (xk + dk) satisfies the Armijo condition 
C              but does not satisfy the beta condition then the point is 
C              accepted, but if it satisfied the Armijo condition and 
C              also satisfies the beta condition then we know that there 
C              is the possibility for a successful extrapolation
C
C              RECOMMENDED: beta = 0.5
C
C     sigma1   double precision
C     sigma2   double precision
C              constant for the safeguarded interpolation
C              if alpha_new \notin [sigma1, sigma*alpha] then we take
C              alpha_new = alpha / nint
C
C              RECOMMENDED: sigma1 = 0.1 and sigma2 = 0.9
C
C     sterel   double precision
C     steabs   double precision
C              this constants mean a ``relative small number'' and ``an 
C              absolute small number'' for the increments in finite
C              difference approximations of derivatives
C
C              RECOMMENDED: epsrel = 10^{-7}, epsabs = 10^{-10} 
C
C     epsrel   double precision
C     epsabs   double precision
C     infrel   double precision
C     infabs   double precision
C              this constants mean a ``relative small number'', ``an 
C              absolute small number'', and ``infinite or a very big
C              number''. Basically, a quantity A is considered 
C              negligible with respect to another quantity B if 
C              |A| < max ( epsrel * |B|, epsabs ) 
C
C              RECOMMENDED: epsrel = 10^{-10}, epsabs = 10^{-20}, 
C                           infrel = 10^{+20}, infabs = 10^{+99}
C
C     On Return
C
C     x        double precision x(n)
C              new current point
C
C     f        double precision
C              functional value at x
C
C     g        double precision g(n)
C              gradient vector at x
C
C     fcnt     integer
C              number of functional evaluations used in this line search
C
C     gcnt     integer
C              number of gradient evaluations used in this line search
C
C     intcnt   integer
C              number of interpolations
C
C     exgcnt   integer
C              number of good extrapolations
C
C     exbcnt   integer
C              number of bad extrapolations
C
C     inform   integer
C              This output parameter tells what happened in this 
C              subroutine, according to the following conventions:
C
C              0 = convergence with an Armijo-like criterion
C                  (f(xnew) <= f(x) + 1.0d-4 * alpha * <g,d>);
C
C              4 = the algorithm stopped because the functional value
C                  is very small (f <= fmin);
C
C              6 = so small step in the line search. After having made 
C                  at least mininterp interpolations, the steplength 
C                  becames small. ``small steplength'' means that we are 
C                  at point x with direction d and step alpha, and, for 
C                  all i, 
C
C                  |alpha * d(i)| .le. max ( epsrel * |x(i)|, epsabs ). 
C 
C                  In that case failure of the line search is declared 
C                  (may be the direction is not a descent direction 
C                  due to an error in the gradient calculations). Use
C                  mininterp > maxfc for inhibit this criterion;
C
C              8 = it was achieved the maximum allowed number of
C                  function evaluations (maxfc);
C
C            < 0 = error in evalf or evalg subroutines.

C     LOCAL SCALARS
      logical samep
      integer extrap,i,interp
      double precision alpha,atmp,fbext,fplus,ftmp,gptd,gtd

C     ==================================================================
C     Initialization 
C     ==================================================================

C     ==================================================================
C     Compute directional derivative
C     ==================================================================

      gtd = 0.0d0
      do i = 1,nind
          gtd = gtd + g(i) * d(i)
      end do

C     ==================================================================
C     Compute first trial
C     ==================================================================

      alpha = min( 1.0d0, amax )

      do i = 1,nind
          xplus(i) = x(i) + alpha * d(i)
      end do

      if ( alpha .eq. amax ) then
          if ( rbdtype .eq. 1 ) then
              xplus(rbdind) = l(rbdind)
          else ! if (rbdtype.eq.2) then
              xplus(rbdind) = u(rbdind)
          end if
      end if

      call calcf(nind,ind,xplus,n,x,m,lambda,rho,fplus,inform)
      fcnt = fcnt + 1

      if ( inform .lt. 0 ) then
 
          if ( iprint .ge. 4 ) then
              write(*, 1000) inform
              write(10,1000) inform
          end if

          return

      end if

C     Print initial information

      if ( iprint .ge. 4 ) then
          write(*, 980) amax
          write(*, 999) alpha,fplus,fcnt

          write(10,980) amax
          write(10,999) alpha,fplus,fcnt
      end if

C     ==================================================================
C     Test Armijo and beta-condition and decide for accepting the trial 
C     point, interpolate or extrapolate.
C     ==================================================================

      if ( amax .gt. 1.0d0 ) then

C         x + d belongs to the interior of the feasible set
          if ( iprint .ge. 4 ) then
              write(*, *) '     x+d belongs to int of the feasible set'
              write(10,*) '     x+d belongs to int of the feasible set'
          end if

C         Verify Armijo

          if ( fplus .le. f + gamma * alpha * gtd ) then

C             Armijo condition holds  
              if ( iprint .ge. 4 ) then
                  write(*, *) '     Armijo condition holds' 
                  write(10,*) '     Armijo condition holds' 
              end if

              if ( gtype .eq. 0 ) then
                  call calcg(nind,ind,xplus,n,x,m,lambda,rho,g,inform)
              else if ( gtype .eq. 1 ) then
                  call calcgdiff(nind,ind,xplus,n,x,m,lambda,rho,g,
     +            sterel,steabs,inform)
              end if
              gcnt = gcnt + 1

              if ( inform .lt. 0 ) then

                  if ( iprint .ge. 4 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return

              end if

              gptd = 0.0d0
              do i = 1,nind
                  gptd = gptd + g(i) * d(i)
              end do

C             Verify directional derivative (beta condition)

              if ( gptd .lt. beta * gtd ) then

C                 Extrapolate
                  if ( iprint .ge. 4 ) then
                      write(*, *)'     The beta-condition does not hold'
                      write(*, *)'     We will extrapolate'
                      write(10,*)'     The beta-condition does not hold'
                      write(10,*)'     We will extrapolate'
                  end if

C                 f and x before extrapolation
                  fbext = fplus

                  do i = 1,nind
                      xbext(i) = xplus(i)
                  end do

                  go to 100

              else

C                 Step = 1 was ok, finish the line search
                  if ( iprint .ge. 4 ) then
                      write(*, *) '     The beta condition is also true'
                      write(*, *) '     Line search is over'
                      write(10,*) '     The beta condition is also true'
                      write(10,*) '     Line search is over'
                  end if

                  f = fplus

                  do i = 1,nind
                      x(i) = xplus(i)
                  end do

                  inform = 0

                  if ( iprint .ge. 4 ) then
                      write(*, 990) inform
                      write(10,990) inform
                  end if

                  go to 500

              end if

          else 

C             Interpolate
              if ( iprint .ge. 4 ) then
                  write(*, *) '     Armijo does not hold'
                  write(*, *) '     We will interpolate'
                  write(10,*) '     Armijo does not hold'
                  write(10,*) '     We will interpolate'
              end if

              go to 200

          end if

      else

C         x + d does not belong to the feasible set (amax <= 1)
          if ( iprint .ge. 4 ) then
              write(*, *) '     x+d does not belong to box-interior'
              write(10,*) '     x+d does not belong to box-interior'
          end if

          if ( fplus .lt. f ) then

C             Extrapolate
              if ( iprint .ge. 4 ) then
                  write(*, *) '     f(x+d) < f(x)'
                  write(*, *) '     We will extrapolate'
                  write(10,*) '     f(x+d) < f(x)'
                  write(10,*) '     We will extrapolate'
              end if

C             f and x before extrapolation
              fbext = fplus

              do i = 1,nind
                  xbext(i) = xplus(i)
              end do

              go to 100

          else 

C             Interpolate
              if ( iprint .ge. 4 ) then
                  write(*, *) '     f(x+d) >= f(x)'
                  write(*, *) '     We will interpolate'
                  write(10,*) '     f(x+d) >= f(x)'
                  write(10,*) '     We will interpolate'
              end if

              go to 200

          end if

      end if


C     ==================================================================
C     Extrapolation
C     ==================================================================

 100  continue

      extrap = 0

C     Test f going to -inf

 120  if ( fplus .le. fmin ) then

C         Finish the extrapolation with the current point

          f = fplus

          do i = 1,nind
              x(i) = xplus(i)
          end do

          if ( extrap .ne. 0 .or. amax .le. 1.0d0 ) then

              if ( gtype .eq. 0 ) then
                  call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
              else if ( gtype .eq. 1 ) then
                  call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,sterel,
     +            steabs,inform)
              end if
              gcnt = gcnt + 1

              if ( inform .lt. 0 ) then
 
                  if ( iprint .ge. 4 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return

              end if

              if ( f .lt. fbext ) then
                  exgcnt = exgcnt + 1
              else
                  exbcnt = exbcnt + 1
              end if

          end if

          inform = 4

          if ( iprint .ge.3 ) then
              write(*, 994) inform
              write(10,994) inform
          end if

          go to 500

      end if

C     Test maximum number of functional evaluations

      if ( fcnt .ge. maxfc ) then

C         Finish the extrapolation with the current point

          f = fplus

          do i = 1,nind
              x(i) = xplus(i)
          end do

C         If extrap=0 and amax>1 the gradient was computed for testing 
C         the beta condition and it is not necessary to compute it again
          if ( extrap .ne. 0 .or. amax .le. 1.0d0 ) then

              if ( gtype .eq. 0 ) then
                  call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
              else if ( gtype .eq. 1 ) then
                  call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,sterel,
     +            steabs,inform)
              end if
              gcnt = gcnt + 1

              if ( inform .lt. 0 ) then

                  if ( iprint .ge. 4 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return

              end if

              if ( f .lt. fbext ) then
                  exgcnt = exgcnt + 1
              else
                  exbcnt = exbcnt + 1
              end if

          end if

          inform = 8

          if ( iprint .ge. 4 ) then
              write(*, 998) inform
              write(10,998) inform
          end if

          go to 500

      end if

C     Test if the maximum number of extrapolations was exceeded

      if ( extrap .ge. maxextrap ) then

C         Finish the extrapolation with the current point

          f = fplus

          do i = 1,nind
              x(i) = xplus(i)
          end do

C         If extrap=0 and amax>1 the gradient was computed for testing 
C         the beta condition and it is not necessary to compute it again
          if ( extrap .ne. 0 .or. amax .le. 1.0d0 ) then

              if ( gtype .eq. 0 ) then
                  call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
              else if ( gtype .eq. 1 ) then
                  call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,sterel,
     +            steabs,inform)
              end if
              gcnt = gcnt + 1

              if ( inform .lt. 0 ) then

                  if ( iprint .ge. 4 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return

              end if

              if ( f .lt. fbext ) then
                  exgcnt = exgcnt + 1
              else
                  exbcnt = exbcnt + 1
              end if

          end if

          inform = 7

          if ( iprint .ge. 4 ) then
              write(*, 997) inform
              write(10,997) inform
          end if

          go to 500

      end if

C     Chose new step 

      if ( alpha .lt. amax .and. next * alpha .gt. amax ) then
          atmp = amax
      else
          atmp = next * alpha
      end if

C     Compute new trial point

      do i = 1,nind
          xtmp(i) = x(i) + atmp * d(i)
      end do

      if ( atmp .eq. amax ) then
          if ( rbdtype .eq. 1 ) then
              xtmp(rbdind) = l(rbdind)
          else ! if ( rbdtype .eq. 2 ) then
              xtmp(rbdind) = u(rbdind)
          end if
      end if

C     Project

      if ( atmp .gt. amax ) then
          do i = 1,nind
              xtmp(i) = max( l(i), min( xtmp(i), u(i) ) )
          end do
      end if

C     Test if this is not the same point as the previous one.
C     This test is performed only when alpha > amax.

      if( alpha .gt. amax ) then

          samep = .true.
          do i = 1,nind
              if ( abs( xtmp(i) - xplus(i) ) .gt.
     +             max( epsrel * abs( xplus(i) ), epsabs ) ) then
                  samep = .false.
              end if
          end do

          if ( samep ) then

C             Finish the extrapolation with the current point

              f = fplus

              do i = 1,nind
                  x(i) = xplus(i)
              end do

C             If extrap=0 and amax>1 the gradient was computed for 
C             testing the beta condition and it is not necessary to 
C             compute it again
              if ( extrap .ne. 0 .or. amax .le. 1.0d0 ) then

                  if ( gtype .eq. 0 ) then
                      call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
                  else if ( gtype .eq. 1 ) then
                      call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,
     +                sterel,steabs,inform)
                  end if
                  gcnt = gcnt + 1

                  if ( inform .lt. 0 ) then

                      if ( iprint .ge. 4 ) then
                          write(*, 1000) inform
                          write(10,1000) inform
                      end if

                      return

                  end if

                  if ( f .lt. fbext ) then
                      exgcnt = exgcnt + 1
                  else
                      exbcnt = exbcnt + 1
                  end if

              end if

              inform = 0

              if ( iprint .ge. 4 ) then
                  write(*, 990) inform
                  write(10,990) inform
              end if

              go to 500

          end if

      end if

C     Evaluate function

      call calcf(nind,ind,xtmp,n,x,m,lambda,rho,ftmp,inform)
      fcnt = fcnt + 1

      if ( inform .lt. 0 ) then

C         if ( iprint .ge. 4 ) then
C             write(*, 1000) inform
C             write(10,1000) inform
C         end if

C         return

C         If the objective function is not well defined in an 
C         extrapolated point, we discard all the extrapolated points
C         and return to a safe region (where the point before
C         starting the extrapolations is)

          f = fbext

          do i = 1,nind
              x(i) = xbext(i)
          end do

C         If extrap=0 and amax>1 the gradient was computed for testing 
C         the beta condition and it is not necessary to compute it again
          if ( extrap .ne. 0 .or. amax .le. 1.0d0 ) then

              if ( gtype .eq. 0 ) then
                  call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
              else if ( gtype .eq. 1 ) then
                  call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,sterel,
     +            steabs,inform)
              end if
              gcnt = gcnt + 1

              if ( inform .lt. 0 ) then

                  if ( iprint .ge. 4 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return

              end if

              exbcnt = exbcnt + 1

          end if

          inform = 0

          if ( iprint .ge. 4 ) then
              write(*, 1010) inform
              write(10,1010) inform
          end if

          go to 500

      end if

C     Print information of this iteration

      if ( iprint .ge. 4 ) then
          write(*, 999) atmp,ftmp,fcnt
          write(10,999) atmp,ftmp,fcnt
      end if

C     If the functional value decreases then set the current point and 
C     continue the extrapolation

      if ( ftmp .lt. fplus ) then

          alpha = atmp

          fplus = ftmp

          do i = 1,nind
              xplus(i) = xtmp(i)
          end do

          extrap = extrap + 1

          go to 120

C     If the functional value does not decrease then discard the last 
C     trial and finish the extrapolation with the previous point

      else

          f = fplus

          do i = 1,nind
              x(i) = xplus(i)
          end do

C         If extrap=0 and amax>1 the gradient was computed for testing 
C         the beta condition and it is not necessary to compute it again
          if ( extrap .ne. 0 .or. amax .le. 1.0d0 ) then

              if ( gtype .eq. 0 ) then
                  call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
              else if ( gtype .eq. 1 ) then
                  call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,sterel,
     +            steabs,inform)
              end if
              gcnt = gcnt + 1

              if ( inform .lt. 0 ) then

                  if ( iprint .ge. 4 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return

              end if

              if ( f .lt. fbext ) then
                  exgcnt = exgcnt + 1
              else
                  exbcnt = exbcnt + 1
              end if

          end if

          inform = 0

          if ( iprint .ge.3 ) then
              write(*, 990) inform
              write(10,990) inform
          end if

          go to 500

      end if
C     ==================================================================
C     End of extrapolation
C     ==================================================================

C     ==================================================================
C     Interpolation
C     ==================================================================

 200  continue

      intcnt = intcnt + 1

      interp = 0

 210  continue

C     Test f going to -inf

      if ( fplus .le. fmin ) then

C         Finish the interpolation with the current point

          f = fplus

          do i = 1,nind
              x(i) = xplus(i)
          end do

          if ( gtype .eq. 0 ) then
              call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
          else if ( gtype .eq. 1 ) then
              call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,sterel,
     +        steabs,inform)
          end if
          gcnt = gcnt + 1

          if ( inform .lt. 0 ) then

              if ( iprint .ge. 4 ) then
                  write(*, 1000) inform
                  write(10,1000) inform
              end if

              return

          end if

          inform = 4

          if ( iprint .ge. 4 ) then
              write(*, 994) inform
              write(10,994) inform
          end if

          go to 500

      end if

C     Test maximum number of functional evaluations

      if ( fcnt .ge. maxfc ) then

C         As this is an abrupt termination then the current point of the 
C         interpolation may be worst than the initial one

C         If the current point is better than the initial one then
C         finish the interpolation with the current point else discard
C         all we did inside this line search and finish with the initial
C         point

          if ( fplus .lt. f ) then

              f = fplus

              do i = 1,nind
                  x(i) = xplus(i)
              end do

              if ( gtype .eq. 0 ) then
                  call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
              else if ( gtype .eq. 1 ) then
                  call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,sterel,
     +            steabs,inform)
              end if
              gcnt = gcnt + 1

              if ( inform .lt. 0 ) then

                  if ( iprint .ge. 4 ) then
                      write(*, 1000) inform
                      write(10,1000) inform
                  end if

                  return

              end if

          end if

          inform = 8

          if ( iprint .ge. 4 ) then
              write(*, 998) inform
              write(10,998) inform
          end if

          go to 500

      end if

C     Test Armijo condition

      if ( fplus .le. f + gamma * alpha * gtd ) then

C         Finish the line search
      
          f = fplus

          do i = 1,nind
              x(i) = xplus(i)
          end do

          if ( gtype .eq. 0 ) then
              call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
          else if ( gtype .eq. 1 ) then
              call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,sterel,
     +        steabs,inform)
          end if
          gcnt = gcnt + 1

          if ( inform .lt. 0 ) then

              if ( iprint .ge. 4 ) then
                  write(*, 1000) inform
                  write(10,1000) inform
              end if

              return

          end if

          inform = 0

          if ( iprint .ge. 4 ) then
              write(*, 990) inform
              write(10,990) inform
          end if

          go to 500

      end if

C     Compute new step

      interp = interp + 1

      if ( alpha .lt. sigma1 ) then
          alpha = alpha / nint      

      else
          atmp = ( - gtd * alpha **2 ) / 
     +           (2.0d0 * ( fplus - f - alpha * gtd ) )

          if ( atmp .lt. sigma1 .or. atmp .gt. sigma2 * alpha ) then
              alpha = alpha / nint

          else
              alpha = atmp
          end if
      end if

C     Compute new trial point

      do i = 1,nind
          xplus(i) = x(i) + alpha * d(i)
      end do

      call calcf(nind,ind,xplus,n,x,m,lambda,rho,fplus,inform)
      fcnt = fcnt + 1

      if ( inform .lt. 0 ) then

          if ( iprint .ge. 4 ) then
              write(*, 1000) inform
              write(10,1000) inform
          end if

          return

      end if

C     Print information of this iteration

      if ( iprint .ge. 4 ) then
          write(*, 999) alpha,fplus,fcnt
          write(10,999) alpha,fplus,fcnt
      end if

C     Test whether at least mininterp interpolations were made and two 
C     consecutive iterates are much close

      samep = .true.
      do i = 1,nind
         if ( abs( alpha * d(i) ) .gt. 
     +        max( epsrel * abs( x(i) ), epsabs ) ) then
             samep = .false.
         end if
      end do

      if ( interp .ge. mininterp .and. samep ) then

C         As this is an abrupt termination then the current point of the 
C         interpolation may be worst than the initial one

C         If the current point is better than the initial one then
C         finish the interpolation with the current point else discard 
C         all we did inside this line search and finish with the initial 
C         point

C         if ( fplus .lt. f ) then

C             f = fplus

C             do i = 1,nind
C                 x(i) = xplus(i)
C             end do

C             if ( gtype .eq. 0 ) then
C                 call calcg(nind,ind,x,n,x,m,lambda,rho,g,inform)
C             else if ( gtype .eq. 1 ) then
C                 call calcgdiff(nind,ind,x,n,x,m,lambda,rho,g,
c    +            sterel,steabs,inform)
C             end if
C             gcnt = gcnt + 1

C             if ( inform .lt. 0 ) then 

C                 if ( iprint .ge. 4 ) then
C                     write(*, 1000) inform
C                     write(10,1000) inform
C                 end if

C                 return

C             end if

C         end if

C         The previous lines were commented because, as it is been used, 
C         this subroutine must return with the initial point in case of 
C         finding a very small interpolation step. From that initial 
C         point, something different will be tried.

          inform = 6

          if ( iprint .ge. 4 ) then
              write(*, 996) inform
              write(10,996) inform
          end if
  
          go to 500

      end if

C     Else, iterate

      go to 210
C     ==================================================================
C     End of interpolation
C     ==================================================================

 500  continue

C     ==================================================================
C     Return
C     ==================================================================

      return

C     Non-executable statements

 980  format(/,6X,'TN Line search (alphamax= ',1PD11.4,')')
 999  format(6X,'Alpha= ',1PD11.4,' F= ',1PD11.4,' FE= ',I5)
 990  format(6X,'Flag of TN Line search= ',I3,
     +          ' (Convergence with an Armijo-like criterion)')
 994  format(6X,'Flag of TN Line search= ',I3,
     +          ' (Small functional value, smaller than ',/,
     +       6X,'parameter fmin)')
 996  format(6X,'Flag of TN Line search= ',I3,
     +          ' (Too small step in the interpolation)')
 997  format(6X,'Flag of TN Line search= ',I3,
     +          ' (Too many extrapolations)')
 998  format(6X,'Flag of TN Line search= ',I3,
     +          ' (Too many functional evaluations)')
 1000 format(6X,'Flag of TN Line search = ',I3,' Fatal Error')
 1010 format(6X,'Flag of TN Line search= ',I3,
     +          ' (Fatal Error in an extrapolated point)')

      end

C     ******************************************************************
C     ******************************************************************

      subroutine calcf(nind,ind,x,n,xc,m,lambda,rho,f,inform)

      implicit none

C     SCALAR ARGUMENTS
      integer nind,n,m,inform
      double precision f

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision x(n),xc(n),lambda(m),rho(m)

C     This subroutines computes the objective function. 
C
C     It is called from the reduced space (dimension nind), expands the
C     point x where the function will be evaluated and call the 
C     subroutine evalf to compute the objective function Finally, 
C     shrinks vector x to the reduced space. 
C
C     About subroutines named calc[something]. The subroutines whos 
C     names start with ``calc'' work in (are called from) the reduced 
C     space. Their tasks are (i) expand the arguments to the full space, 
C     (ii) call the corresponding ``eval'' subroutine (which works in 
C     the full space), and (iii) shrink the parameters again and also 
C     shrink a possible output of the ``eval'' subroutine. Subroutines
C     of this type are: calcf, calcg, calchd, calcgdiff and calchddiff. 
C     The corresponding subroutines in the full space are the user 
C     defined subroutines evalf, evalg and evalhd.

C     LOCAL SCALARS
      integer i

C     Complete x

      do i = nind + 1,n
          x(i) = xc(i)
      end do

C     Expand x to the full space

      call expand(nind,ind,n,x)

C     Compute f calling the user supplied subroutine evalf

      call evalal(n,x,m,lambda,rho,f,inform)

C     Shrink x to the reduced space

      call shrink(nind,ind,n,x)

      return

      end

C     ******************************************************************
C     ******************************************************************

      subroutine calcg(nind,ind,x,n,xc,m,lambda,rho,g,inform)

      implicit none

C     SCALAR ARGUMENTS
      integer nind,n,m,inform

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision x(n),xc(n),lambda(m),rho(m),g(n)

C     This subroutine computes the gradient vector g of the objective 
C     function. 
C
C     It is called from the reduced space (dimension nind), expands the
C     point x where the gradient will be evaluated and calls the user 
C     supplied subroutine evalg to compute the gradient vector. Finally, 
C     shrinks vectors x and g to the reduced space. 
C
C     About subroutines named calc[something]. The subroutines whos 
C     names start with ``calc'' work in (are called from) the reduced 
C     space. Their tasks are (i) expand the arguments to the full space, 
C     (ii) call the corresponding ``eval'' subroutine (which works in 
C     the full space), and (iii) shrink the parameters again and also 
C     shrink a possible output of the ``eval'' subroutine. Subroutines
C     of this type are: calcf, calcg, calchd, calcgdiff and calchddiff. 
C     The corresponding subroutines in the full space are the user 
C     defined subroutines evalf, evalg and evalhd.

C     LOCAL SCALARS
      integer i

C     Complete x

      do i = nind + 1,n
          x(i) = xc(i)
      end do

C     Expand x to the full space

      call expand(nind,ind,n,x)

C     Compute the gradient vector calling the user supplied subroutine 
C     evalg

      call evalnal(n,x,m,lambda,rho,g,inform)

C     Shrink x and g to the reduced space

      call shrink(nind,ind,n,x)
      call shrink(nind,ind,n,g)

      return

      end

C     ******************************************************************
C     ******************************************************************

      subroutine calcgdiff(nind,ind,x,n,xc,m,lambda,rho,g,sterel,steabs,
     +inform)

      implicit none

C     SCALAR ARGUMENTS
      integer nind,n,m,inform
      double precision sterel,steabs

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision x(n),xc(n),lambda(m),rho(m),g(n)

C     This subroutine approximates the gradient vector g of the 
C     objective function in the reduced space using central finite 
C     differences.
C
C     It is called from the reduced space (dimension nind), expands the
C     point x where the gradient will be estimated and calls evalf
C     subroutine (to evaluate the objective function) 2 * nind times.
C     Finally, shrinks vectors x and g to the reduced space. 
C
C     About subroutines named calc[something]. The subroutines whos 
C     names start with ``calc'' work in (are called from) the reduced 
C     space. Their tasks are (i) expand the arguments to the full space, 
C     (ii) call the corresponding ``eval'' subroutine (which works in 
C     the full space), and (iii) shrink the parameters again and also 
C     shrink a possible output of the ``eval'' subroutine. Subroutines
C     of this type are: calcf, calcg, calchd, calcgdiff and calchddiff. 
C     The corresponding subroutines in the full space are the user 
C     defined subroutines evalf, evalg and evalhd.

C     LOCAL SCALARS
      integer i,indi
      double precision fminus,fplus,step,tmp

C     Complete x

      do i = nind + 1,n
          x(i) = xc(i)
      end do

C     Expand x to the full space

      call expand(nind,ind,n,x)
      
C     Approximate the gradient vector by central finite differences

      do i = 1,nind
          indi = ind(i)

          tmp  = x(indi)
          step = max( steabs, sterel * abs( tmp ) )

          x(indi) = tmp + step
          call evalal(n,x,m,lambda,rho,fplus,inform)
          if ( inform .lt. 0 ) then
              return
          end if

          x(indi) = tmp - step
          call evalal(n,x,m,lambda,rho,fminus,inform)
          if ( inform .lt. 0 ) then
              return
          end if

          g(indi) = ( fplus - fminus ) / ( 2.0d0 * step )
          x(indi) = tmp
      end do

C     Shrink x and g to the reduced space

      call shrink(nind,ind,n,x)
      call shrink(nind,ind,n,g)

      return

      end


C     ******************************************************************
C     ******************************************************************

      subroutine calchd(nind,ind,x,d,g,n,xc,m,lambda,rho,hd,xtmp,sterel,
     +steabs,inform)

      implicit none

C     SCALAR ARGUMENTS
      integer inform,m,n,nind
      double precision steabs,sterel

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision d(n),g(n),hd(n),lambda(m),rho(m),x(n),xc(n),
     +        xtmp(n)        

C     This subroutine computes the product Hessian times vector d. As it
C     is called from the reduced space, it expands vectors x and d,  
C     calls the user supplied subroutine evalhd to compute the Hessian 
C     times vector d product, and shrinks vectors x, d and hd. 
C
C     About subroutines named calc[something]. The subroutines whos 
C     names start with ``calc'' work in (are called from) the reduced 
C     space. Their tasks are (i) expand the arguments to the full space, 
C     (ii) call the corresponding ``eval'' subroutine (which works in 
C     the full space), and (iii) shrink the parameters again and also 
C     shrink a possible output of the ``eval'' subroutine. Subroutines
C     of this type are: calcf, calcg, calchd, calcgdiff and calchddiff. 
C     The corresponding subroutines in the full space are the user 
C     defined subroutines evalf, evalg and evalhd.

C     LOCAL SCALARS
      integer i

C     Complete d with zeroes

      do i = nind + 1,n
          d(i) = 0.0d0
      end do

C     Complete x

      do i = nind + 1,n
          x(i) = xc(i)
      end do

C     Expand x and d to the full space

      call expand(nind,ind,n,x)
      call expand(nind,ind,n,d)
      call expand(nind,ind,n,g)

C     Compute the Hessian times vector d product calling the user 
C     supplied subroutine evalhd

      call evalhd(n)

C     Shrink x, d and hd to the reduced space

      call shrink(nind,ind,n,x)
      call shrink(nind,ind,n,d)
      call shrink(nind,ind,n,g)
      call shrink(nind,ind,n,hd)
      
      end

C     ******************************************************************
C     ******************************************************************

      subroutine calchddiff(nind,ind,x,d,g,n,xc,m,lambda,rho,gtype,hd,
     +xtmp,sterel,steabs,inform)

      implicit none

C     SCALAR ARGUMENTS
      integer gtype,inform,m,n,nind
      double precision steabs,sterel

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision d(n),g(n),hd(n),lambda(m),rho(m),x(n),xc(n),
     +        xtmp(n)

C     This subroutine computes the Hessian times vector d product by
C     means of a ``directional finite difference''. The idea is that, at 
C     the current point x, the product H d is the limit of 
C 
C     [ Gradient(x + t d) - Gradient(x) ] / t
C
C     In this implementation we use
C
C     t = max(steabs, sterel ||x||_\infty) / ||d||_\infty
C
C     provided that d is not equal 0, of course. 
C
C     So, we evaluate the Gradient at the auxiliary point x + t d and
C     use the quotient above to approximate H d. To compute the gradient 
C     vector at the auxiliary point it is used evalg or evalgdiff 
C     depending on gtype parameter.
C
C     About subroutines named calc[something]. The subroutines whos 
C     names start with ``calc'' work in (are called from) the reduced 
C     space. Their tasks are (i) expand the arguments to the full space, 
C     (ii) call the corresponding ``eval'' subroutine (which works in 
C     the full space), and (iii) shrink the parameters again and also 
C     shrink a possible output of the ``eval'' subroutine. Subroutines
C     of this type are: calcf, calcg, calchd, calcgdiff and calchddiff. 
C     The corresponding subroutines in the full space are the user 
C     defined subroutines evalf, evalg and evalhd.

C     On Entry
C
C     n        integer
C              order of the x
C
C     x        double precision x(n)
C              point for which Hessian(x) times d will be approximated
C
C     d        double precision d(n)
C              vector for which the Hessian times vetor product will
C              be approximated
C
C     g        double precision g(n)
C              gradient at x
C
C     xtmp     double precision xtmp(n)
C              working vector
C
C     sterel   double precision
C     steabs   double precision
C              these constants mean a ``relative small number'' and 
C              ``an absolute small number''
C
C     On Return
C
C     hd       double precision hd(n)
C              approximation of H d

C     LOCAL SCALARS
      integer flag,i,indi
      double precision dsupn,step,tmp,xsupn

      inform = 0

C     Compute incremental quotients step

      xsupn = 0.0d0
      dsupn = 0.0d0
      do i = 1,nind
          xsupn = max( xsupn, abs( x(i) ) )
          dsupn = max( dsupn, abs( d(i) ) )
      end do

c Safeguard added by LM
      if(dsupn.lt.1.d-20) dsupn = 1.d-20

      step = max( sterel * xsupn, steabs ) / dsupn 

C     Set the point at which the gradient will be evaluated

      do i = 1,nind
          xtmp(i) = x(i) + step * d(i)
      end do

C     Evaluate the gradient at xtmp = x + step * d

      if ( gtype .eq. 0 ) then

C         Complete xtmp

          do i = nind + 1,n
              xtmp(i) = xc(i)
          end do

C         Expand xtmp to the full space

          do i = nind,1,-1
              indi = ind(i)
              if ( i .ne. indi ) then
                  tmp        = xtmp(indi)
                  xtmp(indi) = xtmp(i)
                  xtmp(i)    = tmp
              end if
          end do

c         Compute the gradient at xtmp = x + step * d

          call evalnal(n,xtmp,m,lambda,rho,hd,flag)

C         Shrink hd to the reduced space

          do i= 1, nind
              indi= ind(i)
              if (i.ne.indi) then
                  tmp      = hd(indi)
                  hd(indi) = hd(i)
                  hd(i)    = tmp
              end if    
          end do

      else if ( gtype .eq. 1 ) then

          call calcgdiff(nind,ind,xtmp,n,xc,m,lambda,rho,hd,sterel,
     +    steabs,inform)

      end if

C     Compute incremental quotients

      do i = 1,nind
          hd(i) = ( hd(i) - g(i) ) / step
      end do 

      return

      end


C     ******************************************************************
C     ******************************************************************

      subroutine shrink(nind,ind,n,v)

      implicit none

C     SCALAR ARGUMENTS
      integer n,nind

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision v(n)

C     This subroutine shrinks vector v from the full dimension space 
C     (dimension n) to the reduced space (dimension nind).
C
C     On entry:
C
C     nind     integer 
C              dimension of the reduced space
C
C     ind      integer ind(nind)
C              components ind(1)-th, ..., ind(nind)-th are the
C              components that belong to the reduced space
C
C     n        integer
C              dimension of the full space
C
C     v        double precision v(n)
C              vector to be shrinked
C
C     On Return
C
C     v        double precision v(n)
C              shrinked vector

C     LOCAL SCALARS
      integer i,indi
      double precision tmp

      do i = 1,nind
           indi = ind(i)
           if ( i .ne. indi ) then
               tmp     = v(indi)
               v(indi) = v(i)
               v(i)    = tmp
          end if
      end do

      return

      end

C     ******************************************************************
C     ******************************************************************

      subroutine expand(nind,ind,n,v)

      implicit none

C     SCALAR ARGUMENTS
      integer n, nind

C     ARRAY ARGUMENTS
      integer ind(nind)
      double precision v(n)

C     This subroutine expands vector v from the reduced space 
C     (dimension nind) to the full space (dimension n).
C
C     On entry:
C
C     nind     integer 
C              dimension of the reduced space
C
C     ind      integer ind(nind)
C              components ind(1)-th, ..., ind(nind)-th are the
C              components that belong to the reduced space
C
C     n        integer
C              dimension of the full space
C
C     v        double precision v(n)
C              vector to be expanded
C
C     On Return
C
C     v        double precision v(n)
C              expanded vector

C     LOCAL SCALARS
      integer i,indi
      double precision tmp

      do i = nind,1,- 1
          indi = ind(i)
          if ( i .ne. indi ) then
              tmp     = v(indi)
              v(indi) = v(i)
              v(i)    = tmp
          end if
      end do
     
      return

      end
 
C     ******************************************************************
C     ******************************************************************

      subroutine evalnaldiff(n,x,m,lambda,rho,g,sterel,steabs,inform)

      implicit none

C     SCALAR ARGUMENTS
      integer n,m,inform
      double precision sterel,steabs

C     ARRAY ARGUMENTS
      double precision x(n),lambda(m),rho(m),g(n)

C     Approximates the gradient vector g(x) of the objective function by 
C     central finite differences. This subroutine, which works in the 
C     full space, is prepared to replace the subroutine evalnal (to 
C     evaluate the gradient vector) in the case of the lastest have not
C     being provided by the user.
C
C     On entry:
C
C     n        integer
C              number of variables
C
C     x        double precision x(n)
C              current point
C
C     m        integer
C     lambda   double precision lambda(m)
C     rho      double precision rho(m)
C              These three parameters are not used nor modified by 
C              GENCAN and they are passed as arguments to the user-
C              defined subroutines evalal and evalnal to compute the 
C              objective function and its gradient, respectively. 
C              Clearly, in an Augmented Lagrangian context, if GENCAN is 
C              being used to solve the bound-constrainted subproblems, m 
C              would be the number of constraints, lambda the Lagrange 
C              multipliers approximation and rho the penalty parameters
C
C     sterel   double precision
C              See below
C
C     steabs   double precision
C              This constants mean a ''relative small number'' and ''an 
C              absolute small number'' for the increments in finite 
C              difference approximations of derivatives
C
C              RECOMMENDED: epsrel = 1.0d-07 and epsabs = 1.0d-10 
C
C              CONSTRAINTS: sterel >= steabs > 0
C
C     On Return
C
C     g        double precision g(n)
C              approximation of the gradient vector at x
C
C     inform   integer
C              0 = no errors,
C            < 0 = there was an error in the gradient calculation.

C     LOCAL SCALARS
      integer j
      double precision tmp,step,fplus,fminus

      inform = 0

      do j = 1,n
          tmp  = x(j)
          step = max( steabs, sterel * abs( tmp ) )

          x(j) = tmp + step
          call evalal(n,x,m,lambda,rho,fplus,inform)
          if ( inform .lt. 0 ) then
              return
          end if

          x(j) = tmp - step
          call evalal(n,x,m,lambda,rho,fminus,inform)
          if ( inform .lt. 0 ) then
              return
          end if

          g(j) = ( fplus - fminus ) / ( 2.0d0 * step )
          x(j) = tmp
      end do

      return

      end

C     *****************************************************************
C     *****************************************************************

      double precision function norm2s(n,x)

      implicit none

C     SCALAR ARGUMENTS
      integer n

C     ARRAY ARGUMENTS
      double precision x(n)

C     This subroutine computes the squared Euclidian norm of an 
C     n-dimensional vector.
C
C     On entry:
C
C     n        integer
C              dimension
C
C     x        double precision x(n)
C              vector
C
C     On return:
C
C     The function return the squared Euclidian norm of the 
C     n-dimensional vector x.

      external hsldnrm2
      double precision hsldnrm2

      norm2s = hsldnrm2(n,x,1) ** 2

      return 

      end

C     ******************************************************************
C     ******************************************************************

      DOUBLE PRECISION FUNCTION HSLDNRM2(N,DX,INCX)
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
      DOUBLE PRECISION CUTLO,CUTHI
      PARAMETER (CUTLO=8.232D-11,CUTHI=1.304D19)
      INTEGER INCX,N
      DOUBLE PRECISION DX(*)
      DOUBLE PRECISION HITEST,SUM,XMAX
      INTEGER I,J,NN
      INTRINSIC DABS,DSQRT,FLOAT
      IF (N.GT.0) GO TO 10
      HSLDNRM2 = ZERO
      GO TO 300
   10 CONTINUE
      SUM = ZERO
      NN = N*INCX
      I = 1
   20 CONTINUE
      IF (DABS(DX(I)).GT.CUTLO) GO TO 85
      XMAX = ZERO
      IF (DX(I).EQ.ZERO) GO TO 200
      IF (DABS(DX(I)).GT.CUTLO) GO TO 85
      GO TO 105
  100 I = J
      SUM = (SUM/DX(I))/DX(I)
  105 XMAX = DABS(DX(I))
      GO TO 115
      IF (DABS(DX(I)).GT.CUTLO) GO TO 75
      IF (DABS(DX(I)).LE.XMAX) GO TO 115
      SUM = ONE + SUM* (XMAX/DX(I))**2
      XMAX = DABS(DX(I))
      GO TO 200
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
   75 SUM = (SUM*XMAX)*XMAX
   85 HITEST = CUTHI/DFLOAT(N)
      DO 95 J = I,NN,INCX
        IF (DABS(DX(J)).GE.HITEST) GO TO 100
        SUM = SUM + DX(J)**2
   95 END DO
      HSLDNRM2 = DSQRT(SUM)
      GO TO 300
  200 CONTINUE
      I = I + INCX
      IF (I.LE.NN) GO TO 20
      HSLDNRM2 = XMAX*DSQRT(SUM)
  300 CONTINUE
      RETURN
      END

C     ******************************************************************
C     ******************************************************************
C
C     Report of modifications.
C
C     February 18th, 2005.
C
C     1) An unsed format statement, previously used to automaticaly
C     generates some tables, was deleted.
C
C     2) An unmateched parenthesis was corrected in the format
C     statement used to stop GENCAN due to a small step in a line search.
C
C     February 16th, 2005.
C
C     1) The evalhd subroutine used by default in GENCAN is now the one
C     implemented in calchddiff, which approximates the Hessian-vector
C     product by incremental quotients. The implementation used to 
C     overcome the non twice continuously differentiability of the 
C     classical (PHR) Augmented Lagrangian function is now part of 
C     ALGENCAN (and not GENCAN). So, to use GENCAN inside ALGENCAN, 
C     htvtype argument must be set equal to 0 (ZERO).
C
C     2) The commented version of the empty function evalhd that must
C     be added when GENCAN is beinf used stand-alone was wrong. The
C     arguments declarations had been copied from evalnal. It was 
C     corrected.
C
C     November 10th, 2004.
C
C     1) After several test, all references to nonmontone line search
C     schemes were deleted.
C
C     September 28th, 2004.
C
C     1) Subroutines were checked an some absent arguments explanations
C     were added
C
C     2) Some calling sequences were modified to group related arguments
C
C     3) Arguments and local variables declarations were reordered in
C     alphabetical order.
C
C     3) Shrink and expand subroutines were modified to deal with just
C     one vector at a time. In this way, they are now being called from
C     calc* subroutines.
C
C     September 27th, 2004.
C
C     1) All comments were arranged to fit into the 72-columns format
C
C     2) Unused variable goth, which was prepared to indicate whether 
C     the Hessian matrix have been evaluated at the current point, was 
C     deleted from CG subroutine.
C
C     3) A spell check was used to correct the comments
C
C     September 21th, 2004.
C
C     1) In the stopping criterion where the progress in the objective 
C     function is verified, ''itnfp .ge. maxitnfp'' was changed for 
C     ''itnfp .gt. maxitnfp'', to make the choice maxitnfp equal to 1 
C     sounds reasonable.
C
C     2) Moreover, the previous chance came from the addition in the 
C     comments of GENCAN of the ''constraints'' information which makes 
C     clear to the user the values each argument may assume.
C
C     3) In the calculations of the first ''trust-radius'' for Conjugate 
C     Gradients, ''if( udelta0 .lt. 0.d0 ) then'' was changed by ''if 
C     ( udelta0 .le. 0.0d0 ) then'' to also make the default GENCAN 
C     choice of this initial trust-radius in the case of the user have 
C     been setted udelta = 0 by mistake.
C
C     4) The same for ucgmaxit.
C
C     5) In the line search subroutines spgls and tnls, ''if ( interp 
C     .gt. mininterp .and. samep ) then'' was changes by ''.ge.''.
C
C     6) Some comments of GENCAN arguments were re-written.
C
C     September 16th, 2004.
C
C     1) With the reconfiguration of the calc* subroutines (see (1) 
C     below) there were a number of redundant parameters in calchd and 
C     evalhd subroutines. These parameters were eliminated.
C
C     September 13th, 2004.
C
C     1) Subroutines named calc* that work in the reduced space always
C     call the corresponding eval* subroutine. As it was, calcg (that
C     computes the gradient in the reduced space) called evalg or 
C     evalgdiff depending on gtype parameter. The same was for calchd. 
C     Now, calcg calls evalg, calchd calls evalhd, and calchddiff (new) 
C     approximates the Hessian times vector product by incremental 
C     quotients calling calcg or calcgdiff depending on gtype parameter.
C     An improvement of this modification is that calcg does not call 
C     evalg or evalgdiff (both work in the full space) any more but it 
C     approximates the gradient vector in the reduced space (by central 
C     finite differences) calling 2 * nind times evalf subroutine.
C
C     2) Some comments were added inside evalg and evalhd user supplied
C     subroutines alerting about the relation of these subroutines and
C     the parameters gtype and htvtype, respectively.
C
C     3) Description of tnls subroutine was slightly modified.
C
C     4) The description of htvtype parameter in gencan was again 
C     slightly modified.
C
C     5) With the introduction of the parameter lambda (that in the
C     context of Augmented Lagrangians is used to store the 
C     approximation of the Lagrange multipliers) the name of the 
C     variable used for spectral steplength was changed from lambda to 
C     lamspg. In addition, lammax was changed to lspgma and lammin to 
C     lspgmi.
C
C     6) Modifications introduced in June 15th, 2004 and May 5th, 2004
C     were, in fact, made in this version on September 13th, 2004.
C
C     June 15th, 2004.
C
C     1) The fmin stopping criterion and the maximum number of
C     functional evaluation stopping criterion were erroneously being 
C     tested before the main loop. It was just redundant and, for this 
C     reason, deleted.
C
C     May 5th, 2004.
C
C     1) Incorporated into an Augmented Lagrangian framework.
C
C     a) evalf and evalg were renamed as evalal and evalnal, 
C        respectively.
C
C     b) m,lambda,rho were added as parameters of the subroutines evalal 
C        and evalnal, and, as a consequence, as parameters of almost all 
C        the other subroutines.
C
C     2) The comment of htvtype parameter of gencan was in portuguese
C     and it was translated into english.
C
C     3) A nonmonotone version of gencan is starting to be studied.
C     Parameters p and lastfv(0:p-1) were added to gencan, spgls, and
C     tnls to allow a nonmonotone line search. Array lastfv is now 
C     been updated for saving the last p functional values and the 
C     nonmonotone line searches are been done in a SPG or a 
C     Truncated Newton direction. p = 1 means monotone line search 
C     and is recommended until this study finish.
C
C     April 13th, 2004.
C
C     1) The modifications introduced in the occasion of the IRLOC 
C     development and re-development (October 21th, 2003 and February 
C     19th, 2003, respectively) were in fact made in this version on 
C     April 13th, 2004. The motivation to do this was to unify two 
C     parallel and different version of GENCAN (created, obviously, by 
C     mistake).
C
C     2) The complete reference of the GENCAN paper was finally added.
C
C     May 14th, 2003.
c
C     1) The way amax2 and amax2n were being computing may caused a 
C     segmentation fault. Its initialization was changed from infty and
C     -infty to 1.0d+99 and -1.0d+99, respectively. Using infty, when
C     combined with a big trust region radius, the final value of amax2
C     or amax2n may cause the impression that a bound is being attained, 
C     when it is not. "Redundant" ifs inside the amax2 and anax2n 
C     calculation were deleted. It should considered the possibility of 
C     using two constants, namely, bignum = 1.0d+20 and infty = 1.0d+99, 
C     instead of just infty. 
C
C     Modification introduced in October 21, 2003 in occasion of the
C     IRLOC re-development:
C
C     1) The stooping criteria related to functional value smaller than
C     fmin and exhaustion of maximum allowed number of functional 
C     evaluations have been done after the line search. And the 
C     questions were done as "if line search flag is equal to 4" or "if 
C     line search flag is equal to 8". But it was wrong in the case, for 
C     example, inside the line search, a functional value such that f <= 
C     fmin and the Armijo criterion was satisfied. In such case, the 
C     line search flag was being setted to 0 and not to 4. And gencan 
C     did not stop by the fmin criterion. Now, both stooping criteria 
C     are tested at the begining of the main gencan loop and just the 
C     stooping criteria by small line search step is tested after the 
C     line search.
C
C     Modification introduced in February 19, 2003 in occasion of the
C     IRLOC development:
C
C     1) The description of epsnfp parameter of GENCAN was modified. It
C     was written that to inhibit the related stopping criterion (lack
C     of function progress) it was necessary just set epsnfp = 0 when
C     it is also necessary to set maxitnfp = maxit. it was added in the
C     explanation.
C
C     2) In the explanation at the beginning of GENCAN it was written 
C     that cgscre parameter should be double precision. This comment was 
C     wrong. The correct type for cgscre parameter is integer.
C
C     Modifications introduced near April 1st 2003 in occasion of the 
C     PHR and inequality-constraints Augmented Lagrangian methods 
C     development:
C
C     1) The use of iprint was redefined and iprint2 was deleted.
C
C     2) The way to detect no progress in the log of the projected 
C     gradient norm was changed. As it was, ''no progress'' means no
C     reduction in the projected gradient norm over M iterations.
C     But this criterion implicitly assumed that the projected
C     gradient norm must decrease monotonously. Is it is clearly not
C     true, the criterion was changed by a non-monotone decrease
C     criterion. Now, progress means that the projected gradient
C     norm is, at each iteration, smaller than the maximum over the
C     last M iterations. And "no progress" means the it does not 
C     occurs during  not smaller than the 
C
C     3 ) The computation of qamaxn inside cg subroutine was in the 
C     wrong place (it was being used before computed) and it may was 
C     the reason for which the option nearlyq = .true. never worked 
C     properly. With this correction this option should be tested again.
C
C     On September 29th, 2004, we did a new test using the 41 bound
C     constrained problems with quadratic objective function from the
C     CUTE collection. The behaviour of GENCAN setting nearly equal
C     to true or false was indistinguishable. The test did not
C     include the different choices for the maximum number of CG
C     iterations being restricted to evaluate the different
C     alternatives for the case of finding a direction d such that
C     d^t H d <= 0. As a conclusion of this experiment we continue
C     recommending as a default choice to set nearlyq equal to false.
C
C     Modifications introduced from March 1st to March 21th of 2002
C     in occasion of the ISPG development:
C
C     1) Comments of some new parameters introduced in the previous
C     modification
C
C     2) As it was, in the first iteration of GENCAN (when kappa takes
C     value equal 1) and for one-dimensional faces, cgmaxit(the maximum 
C     number of Conjugate Gradient iterations to compute the internal to
C     the face truncated-Newton direction) was being 0. As it is 
C     obviously wrong, we add a max between what was being computed and 
C     one to allow at least one CG iteration.
C
C     3) Parameter inform in subroutines evalf, evalg and evalhd 
C     supplied by the user was added
C
C     Modifications introduced from May 31th to November 2nd of 2001
C     in occasion of the ALGENCAN development:
C
C     Fixed bugs:
C
C     1) The first spectral steplength was not been projected in the
C     [lspgmi,lspgma] interval.
C
C     2) The conjugate gradients accuracy (cgeps) which is linearly
C     dependent of the Euclidian norm of the projected gradient, was
C     also not been projected in the interval [cgepsi,cgepsf].
C
C     3) Conjugate gradients said that it was being used an Euclidian
C     norm trust region when it has really being used an infinite norm
C     trust region and viceversa.
C
C     4) Sometimes, the analytic gradient has been used although the
C     user choose the finite differences option.
C
C     Modifications:
C
C     1) To avoid roundoff errors, an explicit detection of at least one
C     variable reaching its bound when a maximum step is being made was
C     added.
C
C     2) The way in which two points were considered very similar in, 
C     for example, the interpolations and the extrapolations (which was 
C     dependent of the infinity norm of the points) showed to be very
C     scale dependent. A new version which test the difference 
C     coordinate to coordinate was done. In this was the calculus of the 
C     current point x and the descent direction sup-norm is not done any
C     more.
C
C     3) The same constants epsrel and epsabs were used as small 
C     relative and absolute values for, for example, detecting similar
C     points and for finite differences. Now, epsrel and epsabs are used 
C     for detecting similar points (and the recommended values are 
C     10^{-10} and 10^{-20}, respectively) and new constants sterel and 
C     steabs were introduced for finite differences (and the recommended 
C     values are 10^{-7} and 10^{-10}, respectively).
C
C     4) Two new stopping criteria for CG were added: (i) we stop if
C     two consecutive iterates are too  close; and (ii) we also
C     stop if there is no enough quadratic model progress during
C     maxitnqmp iterations.
C
C     5) The linear relation between the conjugate gradient accuracy
C     and the norm of the projected gradient can be computed using
C     the Euclidian- and the sup-norm of the projected gradient (only
C     Euclidian norm version was present in the previous version. The
C     linear relation is such that the CG accuracy is cgepsi when the
C     projected gradient norm value is equal to the value corresponding
C     to the initial guess and the CG accuracy is cgepsf when the
C     projected gradient norm value is cgrelf).
C
C     6) Inside Conjugate Gradients, the Euclidian-norm is been computed 
C     using an algorithm developed by C.L.LAWSON, 1978 JAN 08. Numerical 
C     experiments showed that the performance of GENCAN depends 
C     basically on the conjugate gradients performance and stopping
C     criteria and that the conjugate gradients depends on the way the
C     Euclidian-norm is been computed. These things deserve further 
C     research.
C
C     7) In the Augmented Lagrangian algorithm ALGENCAN, which uses
C     GENCAN to solve the bounded constrained subproblems, the maximum
C     number of Conjugate Gradients iterations (cgmaxit), which in this
C     version is linearly dependent of the projected gradient norm, was 
C     set to 2 * (# of free variables). As CG is not using restarts we 
C     do not know very well what this means. On the other hand, the 
C     accuracy (given by cgeps) continues being more strict when we are 
C     near to the solution and less strict when we ar far from the 
C     solution. 
c
C     8) Many things in the output were changed.