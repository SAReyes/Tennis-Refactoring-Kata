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
    if (isATie) {
      return translatePoint andThen { _ + "-All" } orElse translateDeuce apply m_score1
    }

    if (aPlayerHasScoredFortyOrMore) {
      val advantageOrWin = computeAdventageOrWin(m_score1, m_score2)
      val winningPlayer = computeWinningPlayer(m_score1, m_score2)

      return s"$advantageOrWin $winningPlayer"
    }

    s"${translateScoreToText(m_score1)}-${translateScoreToText(m_score2)}"
  }

  private def aPlayerHasScoredFortyOrMore = {
    m_score1 >= 4 || m_score2 >= 4
  }

  private def isATie = {
    m_score1 == m_score2
  }
}

object TennisGame1 {
  val Player1Name = "player1"
  val Player2Name = "player2"

  private def isPlayerOne(playerName: String) = {
    playerName == Player1Name
  }

  private def translateScoreToText(tempScore: Int) = {
    translatePoint orElse translateForty apply tempScore
  }

  private val translatePoint: PartialFunction[Int, String] = {
    case 0 => "Love"
    case 1 => "Fifteen"
    case 2 => "Thirty"
  }

  private val translateForty: PartialFunction[Int, String] = {
    case 3 => "Forty"
  }
  private val translateDeuce: PartialFunction[Int, String] = {
    case _ => "Deuce"
  }


  private def computeWinningPlayer(m_score1: Int, m_score2: Int): String = {
    m_score1.compareTo(m_score2) match {
      case 1 => Player1Name
      case -1 => Player2Name
      case _ => ""
    }
  }

  private def computeAdventageOrWin(m_score1: Int, m_score2: Int): String = {
    Math.abs(m_score1 - m_score2) match {
      case 1 => "Advantage"
      case _ => "Win for"
    }
  }
}