---
title: Converting Java collections to Scala collections from Java
---

Converting between Java and Scala collections from Scala code is remarkably easy -

```scala
scala> import scala.collection.JavaConverters._

scala> val jList = new java.util.ArrayList[String]

scala> jList.asScala
scala.collection.mutable.Buffer[String] = Buffer()

scala> jList.asScala.toSeq
Seq[String] = Buffer()

scala> val sList = List[String]()
List[String] = List()

scala> sList.asJava
java.util.List[String] = []

scala> val jMap = new java.util.HashMap[String,String]

scala> jMap.asScala
res6: scala.collection.mutable.Map[String,String] = Map()

scala> jMap.asScala.toMap
res7: scala.collection.immutable.Map[String,String] = Map()
```

But doing the same from Java is a little more difficult. We can see what Scala
is doing under the hood by using `reify` then use the same approach from Java -

```scala
scala> import scala.reflect.runtime.{universe => u}

scala> println(u.reify { jList.asScala.toSeq })
Expr[Seq[String]](
  JavaConverters.asScalaBufferConverter($read.jList).asScala.toSeq
)

scala> println(u.reify { jMap.asScala.toMap })
Expr[scala.collection.immutable.Map[String,String]](
  JavaConverters.mapAsScalaMapConverter(
    $read.$iw.$iw.$iw.$iw.jMap
  ).asScala.toMap(Predef.$conforms)
)
```

In order to pull this off in Java, we can just follow the calls presented in the
desugared ASTs from above -

```java
import java.util.*;
import scala.Predef;
import scala.collection.Seq;
import scala.collection.JavaConverters;

public class Main {
  public static void main(String args[]) {
    // Java collections.
    List<String> jList = new ArrayList<>();
    Map<String, String> jMap = new HashMap<>();

    // Convert to Scala collections.
    Seq<String> sList = JavaConverters.asScalaBufferConverter(xs).asScala().toSeq();
    Map<String, String> sMap =
      JavaConverters.mapAsScalaMapConverter(jMap).asScala().toMap(Predef.$conforms());
  }
}
```
