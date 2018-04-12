В scala есть такая особенность, что оператор сравнения позволяет сравнивать разные типы.
Например вы можете написать такой код:
```scala
Option(1) == "1"
```
И максимум, что мы получим - это предупреждение от компилятора.
Вам нужно написать свой type class под название `Eq[A]`,
для которого будет определена операция `def eq(a1: A, a2: A): Boolean`
А также синтаксис, который позволит писать так:
```scala
Option(1) === Option(1)
"str" !== "str1"
true == false
```

И при этом не давать компилироваться следующему коду:
```scala
Option(1) === Option("1")
"str" !== false
```

Должны быть определены экземпляры для вывода:
`Option[A]`, `Map[String, A]`, `Seq[A]`

А также дефолтный Eq для любого типа, реализованный через оператор `==`