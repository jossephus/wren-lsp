class Logger {
  static info(message) {
    System.print("[INFO] " + message)
  }

  static error(message) {
    System.print("[ERROR] " + message)
  }
}

class MathUtils {
  static clamp(value, min, max) {
    if (value < min) return min
    if (value > max) return max
    return value
  }

  static double(n) {
    return n * 2
  }
}


