	>> SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. Chess.
AUTHOR. Colby Schexnayder.
DATE-WRITTEN. 06/13/2024.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT MoveList ASSIGN TO "movelist.txt"
		ORGANIZATION IS INDEXED
		ACCESS MODE IS RANDOM
		RECORD KEY IS IDNum.

DATA DIVISION.
FILE SECTION.
FD MoveList.
01 MoveData.
	02 IDNum PIC 9(4).

WORKING-STORAGE SECTION.
01 ChessBoard.
	02 BIDNum PIC 9(4).
	02 BoardY OCCURS 8 TIMES INDEXED BY Y.
		03 BoardX OCCURS 8 TIMES INDEXED BY X.
			04 Piece.
				05 OWNER PIC A.
					88 Black VALUE 'B'.
					88 White VALUE 'W'.
					88 Empty VALUE ' '.
				05 Symbol PIC A VALUE ' '.
				05 GameValue PIC 9 VALUE 0.
				05 CurrentX PIC 9 VALUE 0.
				05 CurrentY PIC 9 VALUE 0.
				05 Moved PIC 9.
					88 HasMoved VALUE 1.
					88 HasNotMoved VALUE 0.
					
01 BoardWidth PIC 9 VALUE 8.
01 BoardHeight PIC 9 VALUE 8.
01 EmptySpace PIC X(6) VALUE "  0000".

01 SelectedPiece.
	02 SPieceX PIC 9 VALUE 0.
	02 SPieceY PIC 9 VALUE 0.
01 SelectedDestination.
	02 SDestX PIC 9 VALUE 0.
	02 SDestY PIC 9 VALUE 0.

01 PlayerTurn PIC A.
	88 BlacksTurn VALUE 'B'.
	88 WhitesTurn VALUE 'W'.
	
01 WhiteScore PIC 999 VALUE 0.
01 BlackScore PIC 999 VALUE 0.

01 PromotionChoice PIC X VALUE ' '.
01 TmpVar PIC S99V99 VALUE 0.
01 TmpVar2 PIC S99V99 VALUE 0.
01 CheckOrigin PIC 9 VALUE 0.

01 COUNTER PIC 99 VALUE 1.

PROCEDURE DIVISION.

SET X Y TO 1

PERFORM InitBoard VARYING Y FROM 1 BY 1 UNTIL Y > BoardHeight
	AFTER X FROM 1 BY 1 UNTIL X > BoardWidth
	

MOVE 'W' TO PlayerTurn

PERFORM FOREVER
	
	PERFORM displayBoard VARYING Y FROM 1 BY 1 UNTIL Y > BoardHeight
		AFTER X FROM 1 BY 1 UNTIL X > BoardWidth
	
	DISPLAY "Choose piece to move (11 - 88, 99 to Quit)"
	ACCEPT SelectedPiece
	IF SelectedPiece = 99 THEN
		EXIT PERFORM
	END-IF
	DISPLAY "Choose destination (11 - 88)"
	ACCEPT SelectedDestination
	PERFORM checkValidMove
	
END-PERFORM

STOP RUN.

InitBoard.
	EVALUATE Y
		WHEN 1
			EVALUATE X
				WHEN 1
					MOVE "BR5110" TO Piece(X,Y)
				WHEN 2
					MOVE "BN3120" TO Piece(X,Y)
				WHEN 3
					MOVE "BB3130" TO Piece(X,Y)
				WHEN 4
					MOVE "BQ9140" TO Piece(X,Y)
				WHEN 5
					MOVE "BK0150" TO Piece(X,Y)
				WHEN 6
					MOVE "BB3160" TO Piece(X,Y)
				WHEN 7
					MOVE "BN3170" TO Piece(X,Y)
				WHEN 8
					MOVE "BR5180" TO Piece(X,Y)
				WHEN OTHER
					DISPLAY "OUT OF BOUNDS"
			END-EVALUATE
		WHEN 2
			MOVE "BP1210" TO Piece(X,Y)
			MOVE Y TO CurrentY(X,Y)
		WHEN 7
			MOVE "WP1710" TO Piece(X,Y)
			MOVE Y TO CurrentY(X,Y)
		WHEN 8
			EVALUATE X
				WHEN 1
					MOVE "WR5810" TO Piece(X,Y)
				WHEN 2
					MOVE "WN3820" TO Piece(X,Y)
				WHEN 3
					MOVE "WB3830" TO Piece(X,Y)
				WHEN 4
					MOVE "WQ9840" TO Piece(X,Y)
				WHEN 5
					MOVE "WK0850" TO Piece(X,Y)
				WHEN 6
					MOVE "WB3860" TO Piece(X,Y)
				WHEN 7
					MOVE "WN3870" TO Piece(X,Y)
				WHEN 8
					MOVE "WR5880" TO Piece(X,Y)
				WHEN OTHER
					DISPLAY "OUT OF BOUNDS"
			END-EVALUATE
		WHEN OTHER
			MOVE "  0000" TO Piece(X, Y)
			MOVE X TO CurrentX(X, Y)
			MOVE Y TO CurrentY(X, Y)
	END-EVALUATE.


displayBoard.
	DISPLAY "|" OWNER(X, Y) SYMBOL(X, Y) "|" WITH NO ADVANCING
	
	IF x = BoardWidth THEN
		DISPLAY " "
	END-IF.

checkValidMove.
	DISPLAY "Validating move"
	IF SPieceX = SDestX AND SPieceY = SDestY THEN
		DISPLAY "Invalid move"
		EXIT PARAGRAPH
	END-IF
	
	IF SPieceX > BoardWidth OR SPieceX < 1 OR SPieceY > BoardHeight OR SPieceY < 1 THEN
		DISPLAY "Selection Out Of Bounds"
		EXIT PARAGRAPH
	END-IF
	IF SDestX > BoardWidth OR SDestX < 1 OR SDestY > BoardHeight OR SDestY < 1 THEN
		DISPLAY "Destination Out Of Bounds"
		EXIT PARAGRAPH
	END-IF
	
	IF OWNER(SPieceX, SPieceY) NOT EQUALS 'W' THEN
		DISPLAY "Not Your Piece"
		EXIT PARAGRAPH
	END-IF
	
	EVALUATE Symbol(SPieceX, SPieceY)
		WHEN 'P'
			PERFORM pawnMove
			EXIT PARAGRAPH
		WHEN 'N'
			PERFORM knightMove
			EXIT PARAGRAPH
		WHEN 'B'
			PERFORM bishopMove
			EXIT PARAGRAPH
		WHEN 'R'
			PERFORM rookMove
			EXIT PARAGRAPH
		WHEN 'Q'
			PERFORM queenMove
			EXIT PARAGRAPH
		WHEN 'K'
			PERFORM kingMove
			EXIT PARAGRAPH
	END-EVALUATE.

knightMove.
	IF SPieceY - SDestY = 2 OR SDestY - SPieceY = 2 THEN
		IF SPieceX - SDestX  = 1 OR SDestX - SPieceX = 1 THEN
			IF OWNER(SDestX, SDestY) = ' ' THEN
				PERFORM movePiece
				EXIT PARAGRAPH
			END-IF
			IF OWNER(SDestX, SDestY) = 'B' THEN
				PERFORM takePiece
				EXIT PARAGRAPH
			END-IF
		END-IF
	END-IF
	IF SPieceX - SDestX = 2 OR SDestX - SPieceX = 2 THEN
		IF SPieceY - SDestY  = 1 OR SDestY - SPieceY = 1 THEN
			IF OWNER(SDestX, SDestY) = ' ' THEN
				PERFORM movePiece
				EXIT PARAGRAPH
			END-IF
			IF OWNER(SDestX, SDestY) = 'B' THEN
				PERFORM takePiece
				EXIT PARAGRAPH
			END-IF
		END-IF
	END-IF
	DISPLAY "Invalid knight move".
	
bishopMove.
	COMPUTE TmpVar EQUAL (SPieceY - SDestY) / (SPieceX - SDestX)
	IF TmpVar = 1 OR TmpVar = -1 THEN
		IF SDestY < SPieceY THEN
			MOVE -1 TO TmpVar
		ELSE
			MOVE 1 TO TmpVar
		END-IF
		
		IF SDestX < SPieceX THEN
			MOVE -1 TO TmpVar2
		ELSE
			MOVE 1 TO TmpVar2
		END-IF
		IF SPieceX - SDestX > 1 OR SPieceX - SDestX < -1 THEN		
			PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL Y = SDestY
			
				IF OWNER(X, Y) NOT EQUALS ' ' THEN
					DISPLAY "Invalid bishop move"
					EXIT PARAGRAPH
				END-IF
			
			END-PERFORM		
		END-IF
		IF OWNER(SDestX, SDestY) = ' ' THEN
			PERFORM movePiece
			EXIT PARAGRAPH
		END-IF
		IF OWNER(SDestX, SDestY) = 'B' THEN
			PERFORM takePiece
			EXIT PARAGRAPH
		END-IF
	END-IF
	DISPLAY "Invalid bishop move".
	
rookMove.
	IF SDestX - SPieceX = 0 THEN
		COMPUTE TmpVar EQUAL SDestY - SPieceY
		IF TmpVar > 0 THEN
			MOVE 1 TO TmpVar
		ELSE
			MOVE -1 TO TmpVar
		END-IF
		COMPUTE CheckOrigin EQUAL SPieceY + TmpVar
		
		PERFORM VARYING Y FROM CheckOrigin BY TmpVar UNTIL Y = SDestY
			
			IF OWNER(SPieceX, Y) NOT EQUALS ' ' THEN
				DISPLAY "Invalid rook move"
				EXIT PARAGRAPH
			END-IF
			IF OWNER(SDestX, SDestY) = 'B' THEN
				PERFORM takePiece
				EXIT PARAGRAPH
			END-IF
			IF OWNER(SDestX, SDestY) = ' ' THEN
				PERFORM movePiece
				EXIT PARAGRAPH
			END-IF
		END-PERFORM
	END-IF
	IF SDestY - SDestY = 0 THEN
		COMPUTE TmpVar EQUAL SDestX - SPieceX
		IF TmpVar > 0 THEN
			MOVE 1 TO TmpVar
		ELSE
			MOVE -1 TO TmpVar
		END-IF
		COMPUTE CheckOrigin EQUAL SPieceX + TmpVar
		
		PERFORM VARYING X FROM CheckOrigin BY TmpVar UNTIL X = SDestX
			
			IF OWNER(X, SDestY) NOT EQUALS ' ' THEN
				DISPLAY "Invalid rook move"
				EXIT PARAGRAPH
			END-IF
			IF OWNER(SDestX, SDestY) = 'B' THEN
				PERFORM takePiece
				EXIT PARAGRAPH
			END-IF
			IF OWNER(SDestX, SDestY) = ' ' THEN
				PERFORM movePiece
				EXIT PARAGRAPH
			END-IF
		END-PERFORM
	END-IF
	DISPLAY "Invalid rook move".
	
queenMove.
	PERFORM bishopMove
	PERFORM rookMove.
	
kingMove.
	COMPUTE TmpVar EQUAL SDestX - SPieceX
	COMPUTE TmpVar2 EQUAL SDestY - SPieceY
	
	IF TmpVar < 2 OR TmpVar > -2 THEN
		IF TmpVar2 < 2 OR TmpVar > -2 THEN
			IF OWNER(SDestX, SDestY) = ' ' THEN
				PERFORM movePiece
				EXIT PARAGRAPH
			END-IF
			
			IF OWNER(SDestX, SDestY) = 'B' THEN
				PERFORM takePiece
				EXIT PARAGRAPH
			END-IF
		END-IF
	END-IF
	
	*>Castling
	IF HasNotMoved(SPieceX, SPieceY) AND HasNotMoved(SDestX, SDestY) THEN
		IF SDestX = 1 AND SDestY = SPieceY THEN
			PERFORM VARYING X FROM 2 BY 1 UNTIL X = SPieceX
				IF OWNER(X, SDestY) NOT EQUAL ' ' THEN
					DISPLAY "Cannot castle"
					EXIT PARAGRAPH
				END-IF
				
				MOVE Piece(1,8) TO Piece(4, 8)
				MOVE EmptySpace TO Piece(1, 8)
				MOVE Piece(SPieceX, SPieceY) TO Piece(3, 8)
				MOVE EmptySpace TO Piece(SPieceX, SPieceY)
			END-PERFORM
		END-IF
		IF SDestX = 8 AND SDestY = SPieceY THEN
			PERFORM VARYING X FROM 7 BY -1 UNTIL X = SPieceX
				IF OWNER(X, SDestY) NOT EQUAL ' ' THEN
					DISPLAY "Cannot castle"
					EXIT PARAGRAPH
				END-IF
				
				MOVE Piece(8,8) TO Piece(6, 8)
				MOVE EmptySpace TO Piece(8, 8)
				MOVE Piece(SPieceX, SPieceY) TO Piece(7, 8)
				MOVE EmptySpace TO Piece(SPieceX, SPieceY)
			END-PERFORM
		END-IF
	END-IF
	DISPLAY "Invalid king move".

pawnMove.
	IF SDestX - SPieceX = 0 THEN
		IF SPieceY - SDestY = 1 THEN
			IF OWNER(SDestX, SDestY) = ' ' THEN
				PERFORM movePiece
				PERFORM promotePawn
				EXIT PARAGRAPH
			END-IF
		END-IF
		IF SPieceY - SDestY = 2 AND HasNotMoved(SPieceX, SPieceY) THEN
			IF OWNER(SDestX, SDestY) = ' ' AND OWNER(SDestX, SDestY - 1) = ' 'THEN
				PERFORM movePiece
				PERFORM promotePawn
				EXIT PARAGRAPH
			END-IF
		END-IF
	END-IF
	IF SDestX - SPieceX = 1 OR SPieceX - SDestX = 1 THEN
		IF SPieceY - SDestY = 1 AND OWNER(SDestX, SDestY) = 'B' THEN
			PERFORM takePiece
			PERFORM promotePawn
			EXIT PARAGRAPH
		END-IF
	END-IF
	DISPLAY "Invalid Pawn Move".

takePiece.
	DISPLAY OWNER(SPieceX, SPieceY) Symbol(SPieceX, SPieceY) " takes " 
					OWNER(SDestX, SDestY) Symbol(SDestX, SDestY)
	COMPUTE WhiteScore EQUAL WhiteScore + GameValue(SDestX, SDestY)
	PERFORM movePiece.
			
movePiece.
	MOVE Piece(SPieceX, SPieceY) TO Piece(SDestX, SDestY)
	MOVE EmptySpace TO Piece(SPieceX, SPieceY).

promotePawn.
	IF SDestY = 1 THEN
		DISPLAY "Promote pawn (N, B, R, or Q): " WITH NO ADVANCING
		ACCEPT PromotionChoice
		EVALUATE PromotionChoice
			WHEN "N"
				MOVE "N" TO Symbol(SDestX, SDestY)
				MOVE 3 TO GameValue(SDestX, SDestY)
			WHEN "B"
				MOVE "B" TO Symbol(SDestX, SDestY)
				MOVE 3 TO GameValue(SDestX, SDestY)
			WHEN "E"
				MOVE "R" TO Symbol(SDestX, SDestY)
				MOVE 5 TO GameValue(SDestX, SDestY)
			WHEN "Q"
				MOVE "Q" TO Symbol(SDestX, SDestY)
				MOVE 9 TO GameValue(SDestX, SDestY)
		END-EVALUATE
	END-IF.

END PROGRAM Chess.
