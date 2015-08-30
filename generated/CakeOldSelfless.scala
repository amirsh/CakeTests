object OldSelfless {
  trait Base {
    class Rep[+T]
    def lift[T](t: T): Rep[T] = new Rep[T]
  }
  trait Ops1 extends Base {
    class T1
    implicit class Rep1(self: Rep[T1]) {
      def m1: Rep[T1] = T1d1(self)
      def m2: Rep[T1] = T1d2(self)
    }
    def T1d1(x: Rep[T1]): Rep[T1] = x
    def T1d2(x: Rep[T1]): Rep[T1] = x
  }
  trait Ops2 extends Base {
    class T2
    implicit class Rep2(self: Rep[T2]) {
      def m1: Rep[T2] = T2d1(self)
      def m2: Rep[T2] = T2d2(self)
    }
    def T2d1(x: Rep[T2]): Rep[T2] = x
    def T2d2(x: Rep[T2]): Rep[T2] = x
  }
  trait Ops3 extends Base {
    class T3
    implicit class Rep3(self: Rep[T3]) {
      def m1: Rep[T3] = T3d1(self)
      def m2: Rep[T3] = T3d2(self)
    }
    def T3d1(x: Rep[T3]): Rep[T3] = x
    def T3d2(x: Rep[T3]): Rep[T3] = x
  }
  class Cake1 extends Ops1 with Ops2 with Ops3 {
    lift(new T1).m1.m2
    lift(new T2).m1.m2
    lift(new T3).m1.m2
  }
  class Cake2 extends Ops1 with Ops2 with Ops3 {
    lift(new T1).m1.m2
    lift(new T2).m1.m2
    lift(new T3).m1.m2
  }
  class Cake3 extends Ops1 with Ops2 with Ops3 {
    lift(new T1).m1.m2
    lift(new T2).m1.m2
    lift(new T3).m1.m2
  }
  class Cake4 extends Ops1 with Ops2 with Ops3 {
    lift(new T1).m1.m2
    lift(new T2).m1.m2
    lift(new T3).m1.m2
  }
}

