import java.io.InputStream


object Parse extends App {

  import fastparse.all._


  val stream : InputStream = getClass.getResourceAsStream("/example.txt")
  val lines: Iterator[String] = scala.io.Source.fromInputStream( stream ).getLines

  val name = CharPred(_.isUpper).rep(2).rep(1, sep=" ").!
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

  val vegetable =
    """
      |---ARTICHOKES:  MARKET STEADY.  cartons CA Globe green 18s 18.00 one label
      |35.00 24s 18.00-20.00 one label 35.00 30s 14.00-16.00 one label 35.00 36s
      |14.00-15.00 one label 35.00 48s 10.00 sml lse 18.00-20.00 few 25.00 Thornless
      |green 30s 20.00
    """.stripMargin

  /*
  ---ARTICHOKES:
  MARKET STEADY.
  cartons
     CA Globe green
                        18s                                        18.00
        one label 35.00 24s                                        18.00-20.00
        one label 35.00 30s                                        14.00-16.00
        one label 35.00 36s                                        14.00-15.00
        one label 35.00 48s                                        10.00
        sml lse                                                    18.00-20.00 few 25.00
  Thornless green 30s                                              20.00

  ---ASPARAGUS:
  MARKET SLIGHTLY LOWER.
  11 lb cartons bunched
     MX Green
       lge                                             28.00-31.00 mostly 30.00-31.00
  15 lb cartons bunched
     MX Green Tips
       std                                             30.00-31.00 few 24.00


  ---BEANS:
  OFFERINGS LIGHT. MARKET ABOUT STEADY.
  bushel cartons
     CA
       Fava                                                       22.00
  10 lb cartons
     MX
       Haricot Vert (French Type)                                 24.00
  15 lb cartons
     CA
       Brentwood District Cranberry Type                          42.00-45.00
       Italian Type                                                  37.00-38.00
       Wax Type yellow                                               38.00-42.00 mostly 42.00
  30 lb cartons
     CA
       Salinas-Watsonville California Round Green Type            34.50-36.00
  cartons 10 1-lb film bags
     MX
       Haricot Vert (French Type)                                 25.00


  ---BEETS:
  MARKET STEADY.
  cartons bunched
    CA
      Red Type
        12s                                                          15.00
  25 lb film bags
    CA
      Gold Type
        med                                                          18.00-18.50
    MI
      Red Type
        med                                                          15.00
    MX
      Red Type
        med                                                          11.00-12.00
      Gold Type
        med                                                          15.50-16.50
    OR
      Red Type
        med                                                          15.00
      Gold Type
        med                                                          21.00-22.00
    WA
      Red Type
        med                                                          14.00
      Gold Type
        med                                                          18.00


  ---BROCCOLI:
  MARKET ABOUT STEADY.
  cartons
    CA
      bchd 18s                                                          10.00-11.00 mostly 11.00
  20 lb cartons loose
    CA
      Crown Cut                                                    9.50-10.00
      Short Trim                                                   9.50-11.00 mostly 10.00

  ---BRUSSELS SPROUTS:
  MARKET STEADY.
  25 lb cartons
    CA
      jbo                                                          28.00-30.00
      lge                                                          28.00-30.00
  */




  val nameline = name.! ~ P( " " ) ~ priceType.! ~ P(" as of ") ~ date.!
  val provider = P("Provided by:") ~ ws ~ P(CharsWhile(_ != '\n')).rep(sep="\n ").!
  val empty = P("\n").rep(1)
  val phone_number = P("(") ~ CharPred(_.isDigit).rep(3, max=3).! ~ P(")") ~ ws ~  CharPred(_.isDigit).rep(3, max=3).! ~ P("-") ~  CharPred(_.isDigit).rep(4, max=4).!
  val phone_label = StringIn("Phone", "Fax").! ~ P(":")
  val phones = (phone_label ~ ws ~ phone_number.! ~ ws.?).rep() ~ P("\n")
  val label = (CharPred(_.isUpper).rep(2, max=2) ~ P("_") ~ CharPred(_.isUpper).rep(2, max=2) ~ CharPred(_.isDigit).rep(3,max=3)).! ~ P("\n")
  val weather = P("Weather:") ~/ ws ~ CharsWhile(_ != '\n').! ~ P("\n")
  val vegetables = P("VEGETABLES")
  val header = empty ~ nameline ~/ empty ~ provider ~/ empty ~ phones ~/ empty ~ label ~/ empty ~/ weather ~/ empty ~ vegetables ~ empty

  val item = P("---") ~ P(CharsWhile(_ != ':')).!
  val market_condition = P(CharsWhile(_ != '.')).!


  println(header.parse(input))




 // for ( l <- lines ) {
      //val line = l.trim()

    //println ( l )
  //}

}
