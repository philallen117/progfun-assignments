import objsets._

val gizmodoTweets = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)
// val gizmodoTweetSet = TweetReader.toTweetSet(gizmodoTweets)
val g10 = gizmodoTweets.take(10)
val g20 = gizmodoTweets.take(20)
val g30 = gizmodoTweets.take(30)
// val gs10 = TweetReader.toTweetSet(g10)

// lazy val all = TweetReader.allTweets

val e = new Empty

e incl g10.head
e incl g10.head incl g10.tail.head
e incl g10.head incl g10.tail.head incl g10.tail.tail.head
