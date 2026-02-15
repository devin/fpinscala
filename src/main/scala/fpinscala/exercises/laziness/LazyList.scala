package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    LazyList.unfold((n, this)) ((n, as) =>
      (n, as) match
        case (1, Cons(h, t)) =>
          Some((h(), (0, Empty)))
        case (n, Cons(h, t)) if n > 0 =>
          Some((h(), (n-1, t())))
        case _ => None
    )

  @annotation.tailrec
  final def drop(n: Int): LazyList[A] =
    if n <= 0 then this
    else this match
      case Empty => Empty
      case Cons(_, t) => t().drop(n-1)

  def takeWhile(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this)(as =>
      as match
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
    )

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    LazyList.unfold(this)(bs =>
      bs match
        case Empty => None
        case Cons(h, t) => Some((f(h()), t()))
    )

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, as) =>
      if f(a) then
        LazyList.cons(a, as)
      else
        as
    )

  def append[A2 >: A](as: => LazyList[A2]): LazyList[A2] =
    foldRight(as)((a, acc) => LazyList.cons(a, acc))

  def flatMap[B](f: A => LazyList[B]) =
    foldRight(LazyList.empty[B])((a, bs) =>
      f(a).append(bs)
    )

  def zipWith[B, C](bs: LazyList[B], f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, bs))((as, bs) =>

        (as, bs) match
          case (Empty, _) => None
          case (_, Empty) => None
          case (Cons(ah, at), Cons(bh, bt)) =>
            Some((f(ah(), bh()), (at(), bt()))))

  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n+1))

  lazy val fibs: LazyList[Int] =
    def go(c: Int, n: Int): LazyList[Int] =
      cons(c, go(n, c + n))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => LazyList.cons(a, unfold(s)(f))
      case _ => LazyList.empty

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))(state =>
      state match
        case (c, n) => Some((c, (n, c + n)))
    )

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some(n, n + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some(1, ()))
