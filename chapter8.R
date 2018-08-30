# 8.2 Function Arguments

sprintf("Hello %s", "Jared")

sprintf("Hello %s, today is %s", "Jared", "Sunday")

hello_person <- function(name) {
  print(sprintf("Hello %s", name))
}
hello_person("Jared")

hello_person("Bob")

hello_person("Sarah")

hello_person <- function(first, last) {
  print(sprintf("Hello %s %s", first, last))
}

hello_person("Jared", "Lander")
hello_person(first="Jared", last="Lander")

hello_person(last = "Lander", first = "Jared")

hello_person("Jared", last = "Lander")
hello_person(first = "Jared", "Lander")

hello_person(last = "Lander", "Jared")
hello_person(fir="Jared", l="Lander")

## 8.2.1 Default Arguments

hello_person <- function(first, last = "Doe") {
  print(sprintf("Hello %s %s", first, last))
}

hello_person("Jared")  
hello_person("Jared", "Lander")

## 8.2.2 Extra Arguments

hello_person("Jared", extra="Goodbye")

hello_person("Jared", "Lander", "Goodbye")

hello_person <- function(first, last="Doe", ...) {
  print(sprintf("Hello %s %s", first, last))
}

hello_person("Jared", "Lander", "Goodbye")

# 8.3 Return Values

double.num <- function(x) {
  x * 2
}
double.num(5)

double.num <- function(x) {
  return(x * 2)
}
double.num(5)

double.num <- function(x) {
  return(x * 2)
  
  print("Hello!")
  return(17)
}

double.num(5)

# 8.4 do.call

do.call("hello_person", args=list(first= "Jared", last = "Lander"))
do.call(hello_person, args=list(first = "Jared", last = "Lander"))

run.this <- function(x, func=mean) {
  do.call(func, args = list(x))
}

run.this(1:10)
run.this(1:10, mean)
run.this(1:10, sum)
run.this(1:10, sd)

# 8.5 Conclusion