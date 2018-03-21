import scala.swing._
import scala.swing.event._
import java.awt.{ Color, Dimension, Font }
import java.awt.event.{MouseEvent, MouseAdapter}
import javax.swing.border.LineBorder
import javax.swing.ImageIcon
import java.io.File                                                           
import javax.imageio.ImageIO   
import scala.util.Random


class Square(var field: Field, var xSquare: Boolean = false, var oSquare: Boolean = false, var canBePlayed : Boolean = true) extends Panel 
{
	preferredSize = new Dimension(100, 100) // size of each square
	border = Swing.BeveledBorder(Swing.Raised)

	def isSquarePlayed(): Boolean =
	{
		xSquare || oSquare // if the square is marked with a "X" or "O" then it is played
	} 
	
	def isxSquare(): Boolean =
	{
		xSquare
	} 
	
	def setoSquare(value : Boolean) = 
	{
		oSquare = value 
	}
	
	def setxSquare(value : Boolean) = 
	{
		xSquare = value
	}
	
	def isoSquare(): Boolean =
	{
		oSquare
	}

	def changeStatus(value: Boolean) = { canBePlayed = value } // define if the square can be played or not
	
	def getStatus() : Boolean = { canBePlayed }
		
	listenTo(mouse.clicks)
	reactions +=
	{
		case MouseClicked(_, p, _, _, _) =>
		{
			if (!this.isSquarePlayed && this.canBePlayed) // if the square is not played AND if it can be played (<=> the game is still on!)
			{
				// the user plays 
				field.userPlay(this)
				if (field.howManyPlayed!=field.getDimension()*field.getDimension() && canBePlayed) // if the game is not over
					field.computerPlay() // the computer plays
				
				else if(field.howManyPlayed==field.getDimension()*field.getDimension() && !field.playerWon() && !field.computerWon()) // if all the squares are marked and there is no winner
				{
					javax.swing.JOptionPane.showMessageDialog (
						null,
						"Tie Game",
						"You both lost...",
						javax.swing.JOptionPane.PLAIN_MESSAGE
					)
				}
				else if(field.howManyPlayed==field.getDimension()*field.getDimension() && field.playerWon()) // if  all the squares are marked and the player won at the end
				{
					javax.swing.JOptionPane.showMessageDialog (
						null,
						"You won, congrats!",
						"There's a winner",
						javax.swing.JOptionPane.PLAIN_MESSAGE
					)
				}
				// the case where all the squares are marked and the computer won at the end is handled in the ComputerPlay() function
			}
		}	
	}
	
	override def paintComponent(g : Graphics2D)
	{
		if (isxSquare())
		{
			g.clearRect(0 , 0 , size.width, size.height)
			g.drawLine(0, 0,  size.width, size.height)
			g.drawLine(0, size.height,  size.width, 0)
		}
		else if (isoSquare())
		{
			g.clearRect(0 , 0 , size.width, size.height)
			g.drawOval(0,0, size.width, size.height)
			
		}
		else if (!isSquarePlayed())
		{
			g.clearRect(0 , 0 , size.width, size.height)
			// do nothing
		}	
	}
}

class Field(var mainFrame: MainFrame)
{
	var dimension : Int = 3 // the grid is 3*3
	
	def getDimension() : Int = { dimension }
	
	var listOfSquares:Array[Array[Square]] = Array.ofDim[Square](dimension, dimension) // the field is a field of squares
	var grid = new GridPanel(dimension, dimension)
	for (i<-0 to dimension-1)
	{
		for (j<-0 to dimension-1)
		{
			// we create all the squares and we add them to the field
			listOfSquares(i)(j) = new Square(this)
			grid.contents.append(listOfSquares(i)(j))
		}
	}
	mainFrame.contents = grid
	
	// this function returns the number of times the player and the computer have played
	def howManyPlayed() : Int =
	{
		var count : Int = 0
		for (i<-0 to dimension-1)
		{
			for (j<-0 to dimension-1)
			{
				// count the number of squares marked played
				if (listOfSquares(i)(j).isSquarePlayed())
					count+=1
			}
		}
		count
	}

	// the user plays the square he has clicked on
	def userPlay(square : Square) =
	{
		square.xSquare = true
		square.repaint()
	}
	
	// this function returns the Square to play (its coordinates) if the computer can win or -1 if he can't
	def canComputerWin() : Array[Int] = 
	{
		var count : Int = 0
		var indexes : Array[Int] = Array(-1,-1)
		var indexesToRemember : Array[Int] = Array(-1,-1)
		
		// can he win on a line ?
		for (j<-0 to dimension-1)
		{
			for (i<-0 to dimension-1)
			{
				if (listOfSquares(i)(j).isoSquare)
					count+=1
				if (!listOfSquares(i)(j).isoSquare && !listOfSquares(i)(j).isxSquare)
					indexesToRemember = Array(i,j)
			}
			if (count==dimension-1)
				indexes = indexesToRemember
			count=0
			
		}
		
		// can he win on a column ?
		for (i<-0 to dimension-1)
		{
			for (j<-0 to dimension-1)
			{
				if (listOfSquares(i)(j).isoSquare)
					count+=1
				if (!listOfSquares(i)(j).isoSquare && !listOfSquares(i)(j).isxSquare)
					indexesToRemember = Array(i,j)
			}
			if (count==dimension-1)
				indexes = indexesToRemember
			count=0
		}
		
		// can he win on the right diagonal ?
		for (i<-0 to dimension-1)
		{
			if (listOfSquares(i)(i).isoSquare)
				count+=1
			if (!listOfSquares(i)(i).isoSquare && !listOfSquares(i)(i).isxSquare)
				indexesToRemember = Array(i,i)
		}
		if (count==dimension-1)
			indexes = indexesToRemember
		count=0
		
		// can he win on the left diagonal ?
		for (i<-0 to dimension-1)
		{
			if (listOfSquares(i)(dimension-1-i).isoSquare)
				count+=1
			if (!listOfSquares(i)(dimension-1-i).isoSquare && !listOfSquares(i)(dimension-1-i).isxSquare)
				indexesToRemember = Array(i,dimension-1-i)	
		}
		if (count==dimension-1)
			indexes = indexesToRemember
		count=0
		
		indexes
	}
		
	// this function returns the Square to play if the computer can block the player from winning or -1 if he can't
	def canPlayerWin() : Array[Int] = 
	{
		var count : Int = 0
		var indexes : Array[Int] = Array(-1,-1)
		var indexesToRemember : Array[Int] = Array(-1,-1)
		
		// can he win on a line ?
		for (j<-0 to dimension-1)
		{
			for (i<-0 to dimension-1)
			{
				if (listOfSquares(i)(j).isxSquare)
					count+=1
				if (!listOfSquares(i)(j).isxSquare && !listOfSquares(i)(j).isoSquare)
					indexesToRemember = Array(i,j)
			}
			if (count==dimension-1)
				indexes = indexesToRemember
			count=0
		}
		
		// can he win on a column ?
		for (i<-0 to dimension-1)
		{
			for (j<-0 to dimension-1)
			{
				if (listOfSquares(i)(j).isxSquare)
					count+=1
				if (!listOfSquares(i)(j).isxSquare && !listOfSquares(i)(j).isoSquare)
					indexesToRemember = Array(i,j)
			}
			if (count==dimension-1)
				indexes = indexesToRemember
			count=0
		}
		
		// can he win on the right diagonal ?
		for (i<-0 to dimension-1)
		{
			if (listOfSquares(i)(i).isxSquare)
				count+=1
			if (!listOfSquares(i)(i).isxSquare && !listOfSquares(i)(i).isoSquare)
				indexesToRemember = Array(i,i)
		}
		if (count==dimension-1)
			indexes = indexesToRemember
		count=0
		
		// can he win on the left diagonal ?
		for (i<-0 to dimension-1)
		{
			if (listOfSquares(i)(dimension-1-i).isxSquare)
				count+=1
			if (!listOfSquares(i)(dimension-1-i).isxSquare &&  !listOfSquares(i)(dimension-1-i).isoSquare)
				indexesToRemember = Array(i,dimension-1-i)	
		}
		if (count==dimension-1)
			indexes = indexesToRemember
		count=0
		
		indexes
	}
	
	// this function returns true if the player has won
	def playerWon() : Boolean = 
	{
		var count : Int = 0
		var result : Boolean = false
		
		// on a line ?
		for (j<-0 to dimension-1)
		{
			for (i<-0 to dimension-1)
			{
				if (listOfSquares(i)(j).isxSquare)
					count+=1
			}
			if (count==dimension)
				result = true
			count=0
		}
		
		// can he win on a column ?
		for (i<-0 to dimension-1)
		{
			for (j<-0 to dimension-1)
			{
				if (listOfSquares(i)(j).isxSquare)
					count+=1
			}
			if (count==dimension)
				result = true
			count=0
		}
		
		// can he win on the right diagonal ?
		for (i<-0 to dimension-1)
		{
			if (listOfSquares(i)(i).isxSquare)
				count+=1
		}
		if (count==dimension)
			result = true
		count=0
		
		// can he win on the left diagonal ?
		for (i<-0 to dimension-1)
		{
			if (listOfSquares(i)(dimension-1-i).isxSquare)
				count+=1
		}
		if (count==dimension)
			result = true
		count=0
		
		result
	}
	
	// this function returns true if the computer has won
	def computerWon() : Boolean = 
	{
		var count : Int = 0
		var result : Boolean = false
		
		// on a line ?
		for (j<-0 to dimension-1)
		{
			for (i<-0 to dimension-1)
			{
				if (listOfSquares(i)(j).isoSquare)
					count+=1
			}
			if (count==dimension)
				result = true
			count=0
		}
		
		// can he win on a column ?
		for (i<-0 to dimension-1)
		{
			for (j<-0 to dimension-1)
			{
				if (listOfSquares(i)(j).isoSquare)
					count+=1
			}
			if (count==dimension)
				result = true
			count=0
		}
		
		// can he win on the right diagonal ?
		for (i<-0 to dimension-1)
		{
			if (listOfSquares(i)(i).isoSquare)
				count+=1
		}
		if (count==dimension)
			result = true
		count=0
		
		// can he win on the left diagonal ?
		for (i<-0 to dimension-1)
		{
			if (listOfSquares(i)(dimension-1-i).isoSquare)
				count+=1
		}
		if (count==dimension)
			result = true
		count=0
		
		result
	}
	
	// make the computer play
	def computerPlay() =
	{
		// did the player win ?
		// if he did then the game is over
		if (playerWon())
		{
			javax.swing.JOptionPane.showMessageDialog (
				null,
				"You won, congrats!",
				"There's a winner!",
				javax.swing.JOptionPane.PLAIN_MESSAGE
			)
			gameOver()
		}
		
		var indexesToPlayWin : Array[Int] = canComputerWin()
		var i1 = indexesToPlayWin(0)
		var j1 = indexesToPlayWin(1)
		
		
		var indexesToPlayBlock : Array[Int] = canPlayerWin()
		var i2 = indexesToPlayBlock(0)
		var j2 = indexesToPlayBlock(1)
		

		// if the computer can win, he wins
		if (i1!=(-1) && j1!=(-1))
		{
			if (listOfSquares(i1)(j1).getStatus())
			{
				listOfSquares(i1)(j1).setoSquare(true)
				listOfSquares(i1)(j1).repaint()
				
				// did the computer win ?
				if (computerWon())
				{
					javax.swing.JOptionPane.showMessageDialog (
						null,
						"Game Over",
						"Sorry, you lost!",
						javax.swing.JOptionPane.PLAIN_MESSAGE
					)
					gameOver()
				}
			}
		}
		
		// if he can't win, he tries to stop the player from winning
		else if (i2!=(-1) && j2!=(-1))
		{
			if (listOfSquares(i2)(j2).getStatus())
			{
				listOfSquares(i2)(j2).setoSquare(true)
				listOfSquares(i2)(j2).repaint()
			}
		}
		
		// if he can't stop the player from winning, then he plays randomly
		else
		{
			// play randomly
			var r = new scala.util.Random
			var r1 = 0
			var r2 = 0
			do
			{
				r1 = r.nextInt(dimension)
				r2 = r.nextInt(dimension)
			} while (listOfSquares(r1)(r2).isSquarePlayed())
			if (listOfSquares(r1)(r2).getStatus())
			{
				listOfSquares(r1)(r2).setoSquare(true)
				listOfSquares(r1)(r2).repaint()
			}
		}
	}
	
	// the squares can't be marked anymore
	def gameOver() =
	{
		for (i<-0 to dimension-1)
		{
			for (j<-0 to dimension-1)
			{
				listOfSquares(i)(j).changeStatus(false)
			}
		}
	}
	
	// the parameters are set back to default
	def newGame() = 
	{
		for (i<-0 to dimension-1)
		{
			for (j<-0 to dimension-1)
			{
				listOfSquares(i)(j).setoSquare(false)
				listOfSquares(i)(j).setxSquare(false)
				listOfSquares(i)(j).changeStatus(true)
				listOfSquares(i)(j).repaint()
			}
		}
	}

}

object TicTacToeG extends SimpleSwingApplication
{
	def top = new MainFrame
	{
		var field = new Field(this)
		title = "Tic Tac Toe Game"
		menuBar = new MenuBar
		{
			contents += new Menu("Quit")
			{
				contents += new MenuItem(Action("Exit")
				{
				  sys.exit(0)
				})
			}
			contents += new Menu("Start")
			{
				contents += new MenuItem(Action("New Game")
				{
					field.newGame()
				})
			}
		}
		var border = Swing.EmptyBorder(10, 10, 10, 10)
		pack()
		visible = true
	}
}