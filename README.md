# Codec

类型安全使我快乐。Scala使我快乐。

但我是一个没那么快乐的Java boy，写Java时很多字段为了json解析方便我都只敢定义成String类型或者数字类型。  

然后用到的地方再解析字符串或者数字，代码里一堆魔法值到处飞。

要是解析json的时候能直接把字符串一步到位解析成具体的类型多好啊，写一个Codec避免后面每个调用的地方都手动解析一遍。

Java虽然也能写Codec，但是没有隐式参数加持的Codec就不是个好厨师。  

说回Scala，Circe的Codec真好用，但它又只搞json解析。

有时我定义一个新类型，需要为这个新类型给好几个依赖库分别写一份Codec，  

给Circe写一个Codec，给http4s写一个EntityDecoder和一个SegmentDecoder，给doobie写一个Get，给ciris写一个ConfigDecoder

我能不能只写一份Codec然后再给这些库提供一个Codec转换的隐式对象呢，这样我每定义一个新类型只写一份Codec不就行了吗？

于是就有了这个codec库，因为circe真好用，所以核心代码基本照抄了Circe。

但是我为了适配其它库的Codec所以做了些扩展：

1. 把Circe的Encoder[A]扩展成了Encoder[F[_], S, A]表示A => F[S]，这里的S可以是任意类型，当然大部分情况S是String或Json。
2. 把Circe的Decoder[A]扩展成了Decoder[F[_], T, A]表示T => F[Either[DecodingFailure, A]]，这里的T也可以是任意类型，当然大部分情况T是String或Cursor[String]。
3. 把Circe的ACursor扩展成了Cursor[S]，这里的S一般是Json。Circe的ACursor为了兼容性没有sealed起来，我没有兼容性的顾虑直接sealed起来了。
4. 为了把Cursor[S]中的S能扩展出来，我又抽象出了ObjectType、NullType、ArrayType、StringType、NumberType、BooleanType等用于表示S是否支持对象操作、是否可为Null、是否支持数组操作、能否转化字符串、数字、布尔等。
5. 支持derive，使用自己照着shapeless手撸的generic库实现的，Circe不支持case class A(a: Option[A])这样的递归结构，我手撸的generic库用: =>解决掉了。
6. 照抄Circe为Java和Scala的基本类型实现了一套基本的Codec
7. 为自己常用的http4s，doobie等库实现了codec转换，也为一些常用库的数据类型提供了基本的Codec实现
8. 为了将http请求的query参数和urlForm参数解析了实体对象，基本此库实现了query库实现了这个功能，Query和Json可以共用同一个Codec，美滋滋。
