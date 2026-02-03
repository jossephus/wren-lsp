
class A {
  construct new(x) {}
  construct new(x, y, z) {}
  construct def(x) {}

  abc(x) {}
}
class B {}

var fn = Fn.new { |x| System.print(x) }

fn.call(1)

var a = 1

