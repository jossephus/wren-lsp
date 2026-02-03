import "input" for Keyboard
import "graphics" for Canvas, Color

class Main {
  construct new() {}

  init() {
    _x = 10
    _y = 10
    _w = 5
    _h = 5
  }

  update() {
    if (Keyboard.isKeyDown("left")) {
      _x = _x - 1
    }
    if (Keyboard.isKeyDown("right")) {
      _x = _x+ 1
    }
    if (Keyboard.isKeyDown("up")) {
      _y = _y - 1
    }
    if (Keyboard.isKeyDown("down")) {
      _y = _y + 1
    }
  }

  draw(alpha) {
    Canvas.cls()
    var color = Color.rgb(171, 82, 54)
    Canvas.rectfill(_x, _y, _w, _h, color)
  }
}

var Game = Main.new()
