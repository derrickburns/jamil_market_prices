import java.io.InputStream


object Parse extends App {

  import fastparse.all._


  val stream : InputStream = getClass.getResourceAsStream("/example.txt")
  val lines: Iterator[String] = scala.io.Source.fromInputStream( stream ).getLines

  val name: P[String] = CharPred(_.isUpper).rep(2).rep(1, sep=" ").!
  val priceType = P("Terminal Prices").!
  val dayOfMonth = (CharIn('0'to'3').? ~ CharPred(_.isDigit)).!
  val month = StringIn("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV" ,"DEC").!
  val year = P("20").! ~ CharPred(_.isDigit).rep(2).!
  val date = dayOfMonth ~ P("-") ~ month ~ P("-") ~ year

  val ws = CharIn(" \t").rep(1)



  val input = """
                |
                |
                |SAN FRANCISCO Terminal Prices as of 29-AUG-2016
                |
                |Provided by:  Specialty Crops Market News
                |              Federal - State Market News Service, USDA.
                |
                |Phone:  (650) 552-9195     Fax:  (650) 552-9147
                |
                |
                |SX_FV020
                |
                |Weather: 7:00  am  mostly  cloudy  56  yesterday's  high  65  low  59
                |
                |
                |
                |
                |VEGETABLES
                |
                |
                |""".stripMargin


  val nameline = name.! ~ P( " " ) ~ priceType.! ~ P(" as of ") ~ date.!
  val provider = P("Provided by:") ~ ws ~ P(CharsWhile(_ != '\n')).rep(sep="\n ").!
  val empty = P("\n").rep(1)
  val vegetables = P("VEGETABLES")

  val phone_number = P("(") ~ CharPred(_.isDigit).rep(3, max=3).! ~ P(")") ~ ws ~  CharPred(_.isDigit).rep(3, max=3).! ~ P("-") ~  CharPred(_.isDigit).rep(4, max=4).!
  val phone_label = StringIn("Phone", "Fax").! ~ P(":")
  val phones = (phone_label.! ~ ws ~ phone_number.! ~ ws.?).rep() ~ P("\n")

  val label = (CharPred(_.isUpper).rep(2, max=2) ~ P("_") ~ CharPred(_.isUpper).rep(2, max=2) ~ CharPred(_.isDigit).rep(3,max=3)).! ~ P("\n")
  val weather = P("Weather:") ~/ ws ~ CharsWhile(_ != '\n').! ~ P("\n")



  val header = empty ~ nameline.! ~/ empty ~ provider.! ~/ empty ~ phones.! ~/ empty ~ label.! ~/ empty ~/ weather.! ~/ empty ~ vegetables ~ empty


  println(header.parse(input))
  println( (empty ~ nameline).parse(input))




 // for ( l <- lines ) {
      //val line = l.trim()

    //println ( l )
  //}

}
