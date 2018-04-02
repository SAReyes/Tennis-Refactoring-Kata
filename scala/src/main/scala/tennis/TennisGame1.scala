package tennis


class TennisGame1(val player1Name: String, val player2Name: String) extends TennisGame {

  import TennisGame1._

  var m_score1: Int = 0
  var m_score2: Int = 0

  def wonPoint(playerName: String) {
    if (isPlayerOne(playerName))
      m_score1 += 1
    else
      m_score2 += 1
  }

  def calculateScore(): String = {
    var score: String = ""
    var tempScore = 0

    if (isATie) {
      return m_score1 match {
        case 0 => "Love-All"
        case 1 => "Fifteen-All"
        case 2 => "Thirty-All"
        case _ => "Deuce"
      }
    }

    if (aPlayerHasScoredFortyOrMore) {
      val minusResult = m_score1 - m_score2
      if (minusResult == 1) score = "Advantage player1"
      else if (minusResult == -1) score = "Advantage player2"
      else if (minusResult >= 2) score = "Win for player1"
      else score = "Win for player2"

      return score
    }
   
    s"${translateScoreToText(m_score1)}-${translateScoreToText(m_score2)}"
  }

  private def translateScoreToText(tempScore: Int) = {
    tempScore match {
      case 0 => "Love"
      case 1 => "Fifteen"
      case 2 => "Thirty"
      case 3 => "Forty"
    }
  }

  private def aPlayerHasScoredFortyOrMore = {
    m_score1 >= 4 || m_score2 >= 4
  }

  private def isATie = {
    m_score1 == m_score2
  }
}

object TennisGame1 {
  private def isPlayerOne(playerName: String) = {
    playerName == "player1"
  }
}