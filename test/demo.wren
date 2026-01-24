import "./util" for Logger

class Hero {
  construct new(name, score) {
    _name = name
    _score = score
  }

  name { _name }
  score { _score }
}

System.print("ok")
