package sed

class Buffer(s: String) {



	import scala.collection.mutable.StringBuilder



	private var buffer: StringBuilder = new StringBuilder(s)

	private var cursor: Int = 0  // cursor is in between characters

	private var marker: Int = 0  // marker is in between characters

	private var paste: String = ""

	private def end: Int = buffer.length              // the end of the line

	private def lwr: Int = Math.min(marker, cursor)

	private def upr: Int = Math.max(marker, cursor)



	/*

	 * Accessor methods to return aspects of the state

	 */

	def getCursor: Int = cursor

	def getMarker: Int = marker

	def getString: String = buffer.toString

	def getPaste: String = paste



	/**

	 * Repeatedly run a sequence of commands

	 */

	def rpt (n: Int) ( commands: => Unit ) {

		for (i <- 1 to n) { commands }

	}




	/**

	 * AL: Arrow Left.  Move the cursor one place to the left.  If the cursor is

	 * already at position zero then it wraps around to the end of the buffer. 

	 * This operation does not change any other state variables.

	 */

	def al() {
		cursor -= 1

				if (cursor == -1)
					cursor += getString.length() + 1
	}



	/**

	 * AR: Arrow Right.  Move the cursor one place to the right. If the cursor is

	 * already at the end then it wraps around to position zero. 

	 * This operation does not change any other state variables.

	 */

	def ar() {
		cursor += 1
				
		if (cursor > getString.length()) {
					cursor -= getString.length()
					cursor -= 1
			}

		if (getString.length() == 0)
			cursor = getString.length()

	}



	/**

	 * Go to (move) the cursor to a specific position.  This is limited so that if

	 * the requested position is out of the valid cursor range then the cursor is

	 * not moved at all.  That is, only valid moves succeed.

	 * This operation does not change any other state variables.

	 */

	def go(n: Int) {
		if (n <= getString.length && n >= 0) {
			cursor = n
		}
	}



	/**

	 * To Left.  Move the cursor to the left-most (zero) position. This operation

	 * does not change any other state variables.

	 */

	def tl() {
		cursor = 0
	}



	/**

	 * To Right. Move the cursor to the right-most (end of buffer) position. This

	 * operation does not change any other state variables.

	 */

	def tr() {
		cursor = buffer.length 
	}



	/**

	 * Define Region.  Save the current cursor position (as marker). Only changes

	 * the marker. This operation does not change any other state variables.

	 */

	def dr() {
	  marker = cursor
	}



	/**

	 * Define All.  Set the marker to zero, and the cursor to end.  This marks

	 * the entire buffer. This operation does not change the paste buffer state.

	 */

	def da() {
	  marker = 0
	  cursor = buffer.length 
	}



	/**

	 * Enter String. Insert the given string at the current cursor position.

	 * After insertion the cursor moves to the end of the inserted text. 

	 * This operation modifies the string buffer and the cursor position. The

	 * paste buffer and the marker position remain unchanged.

	 */

	def es(s: String) {
	  buffer.insert(cursor, s)
	  cursor += s.length
	}



	/**

	 * Copy the contents of the marked region to the paste buffer. This operation

	 * updates the paste buffer but does not change any other state variables.

	 */

	def xc() {
	  if (marker < cursor) {
	  var xcx: Int = marker
	  while (xcx < cursor) {
	  paste = paste + buffer.charAt(xcx)
	  xcx+=1
	  }
	  }
	  
	  if (cursor < marker) {
	  var xcx: Int = cursor
	  while (xcx < marker) {
	  paste = paste + buffer.charAt(xcx)
	  xcx+=1
	  }
	  }
	}


	/**

	 * Delete the contents of the marked region and save the cut string in the paste

	 * buffer. This operation re-sets the cursor and the marker to the start of the

	 * cut text. 

	 */

	def xd() {
	  
	  
	  if (marker < cursor) {
	  var xcx: Int = marker
	  while (xcx < cursor) {
	  paste = paste + buffer.charAt(xcx)
	  xcx+=1
	  }
	  }
	  
	  if (cursor < marker) {
	  var xcx: Int = cursor
	  while (xcx < marker) {
	  paste = paste + buffer.charAt(xcx)
	  xcx+=1
	  }
	  }
	  
	  if (marker < cursor) {
	  buffer.delete(marker,cursor)
	  cursor = marker
	  }
	  
	  if (marker > cursor) {
	  buffer.delete(cursor,marker)
	  marker = cursor
	  }

	}



	/**

	 * Insert the contents of the paste buffer at the current cursor position.

	 * Effectively, this is the same as calling es() with the contents of the

	 * paste buffer as the string to be inserted. Therefore the cursor aligns

	 * with the end of the inserted string.

	 */

	def xp() {
	  buffer.insert(cursor, paste)
	  cursor += paste.length
	
	}



	/**

	 * Edit Erase  (forward delete).  Delete the character to the right of the

	 * cursor. If the cursor is at the end of the buffer then ignore this command

	 * (there is no character to the right of the cursor). If deletion succeeds

	 * then do not change the cursor position.  The marker position is not

	 * changed UNLESS the deletion has caused it to point beyond the end of the

	 * (updated) buffer. In this case make the marker equal to end.

	 */

	def ee() {
	  buffer.delete(cursor,cursor+1)
	  
	  if (marker > getString.length) {
	    marker = getString.length
	  }
	}



	/**

	 * Edit Delete (backspace/backward delete). Delete the character to the left of

	 * the cursor. If the cursor is at the start of the buffer (index zero) then

	 * ignore this command (there is no character to the left of the cursor). If

	 * deletion succeeds then the cursor is moved one place to the left. The marker

	 * position is not changed UNLESS the deletion has caused it to point beyond the

	 * end of the (updated) buffer. In this case make the marker equal to end.

	 */

	def ed() {
	  if (cursor > 0) {
    buffer.delete(cursor-1,cursor)
    cursor -= 1
	  }
	  
	  if (marker > getString.length) {
	    marker = getString.length
	  }
	}



	/**

	 * Update the buffer by inverting the case of any alphabetic characters within

	 * the defined region.  Only the characters between cursor and marker are

	 * therefore affected.  This operation does not change any other state variables.

	 */

	def cc() {
	  def inverter (x: Char): Char = 
	    if (x.isLower) x.toUpper 
	    else x.toLower
	    var buffs: String = ""
	    
	    
	  if (marker < cursor) {
	  var xcx: Int = marker
	  while (xcx < cursor) {
	  buffs = buffs + buffer.charAt(xcx)
	  xcx+=1
	  }
	  }
	  
	  if (cursor < marker) {
	  var xcx: Int = cursor
	  while (xcx < marker) {
	  buffs = buffs + buffer.charAt(xcx)
	  xcx+=1
	  }
	  }
	   
		buffs = buffs.map (inverter)

		if (cursor < marker) {
		buffer.delete(cursor, marker)
		buffer.insert(cursor, buffs)

		}
		if (cursor > marker) {
		buffer.delete(marker, cursor)
		buffer.insert(marker, buffs)
		}
		
	}



	/**

	 * Substitute Characters.  Within the defined region substitute any instances of

	 * character s with character t. This operation does not change any other state

	 * variables. 

	 */

	def sc(s: Char, t: Char) {
	  	def converter (x: Char, y: Char, z: Char): Char = 
	    if (x == y) z
	    else x
	    var buffs: String = ""
	    
	    
	  if (marker < cursor) {
	  var xcx: Int = marker
	  while (xcx < cursor) {
	  buffs = buffs + converter(buffer.charAt(xcx), s, t)
	  xcx+=1
	  }
	  }
	  
	  if (cursor < marker) {
	  var xcx: Int = cursor
	  while (xcx < marker) {
	  buffs = buffs + converter(buffer.charAt(xcx), s, t)
	  xcx+=1
	  }
	  }
	  

		if (cursor < marker) {
		buffer.delete(cursor, marker)
		buffer.insert(cursor, buffs)

		}
		if (cursor > marker) {
		buffer.delete(marker, cursor)
		buffer.insert(marker, buffs)
		}
	}



	/**

	 * Delete Duplicate characters.  Within the defined region, for each character,

	 * if it occurs once then keep it, but if it occurs multiple times then keep

	 * only the first occurrence.  The characters to the left and right of the

	 * defined region remain unchanged, but within the defined region the duplicates

	 * are removed. This operation does not affect the paste buffer. The marker is

	 * placed finally at the lower end of the defined region and the cursor is placed

	 * finally at the upper end of the (probably reduced) defined region. 

	 */

	def dd() {
	var ddbuffs: String = ""
	var dd1: Int = 0
	var dd2: Int = 0
	
  if (marker < cursor) {
	  var xcx: Int = marker
	  while (xcx < cursor) {
	  ddbuffs = ddbuffs + buffer.charAt(xcx)
	  xcx+=1
	  }
	  }
	  
	  if (cursor < marker) {
	  var xcx: Int = cursor
	  while (xcx < marker) {
	  ddbuffs = ddbuffs + buffer.charAt(xcx)
	  xcx+=1
	  }
	  }
	  
	  dd1 = ddbuffs.length
	  ddbuffs = ddbuffs.distinct
	  dd2 = ddbuffs.length
	  
	  if (marker < cursor) {
	  buffer.delete(marker,cursor)
	  buffer.insert(marker, ddbuffs)
	  cursor -= dd1 - dd2

	  }
	  
	  if (marker > cursor) {
	  buffer.delete(cursor,marker)
    buffer.insert(marker, ddbuffs)
	  }
	}



	/**

	 * Find Forwards.  Starting at the current cursor position, locate the next

	 * occurrence of the character c. If the cursor reaches the end of the string

	 * without finding an occurrence of c then return false and return leave the

	 * cursor position unchanged. If the cursor finds an occurrence of c then leave

	 * the cursor at that position and return true.

	 * This operation does not change any variable other than the cursor position.

	 */

	def ff(c: Char): Boolean = {
	  var check: Boolean = false
	  var buffs: String = ""

	  
	  var xcx = cursor
	  while (xcx < buffer.length) {
	  buffs = buffs + buffer.charAt(xcx)
	  xcx+=1
	  }
	  xcx = cursor

	  
	  while (xcx < buffs.length|| check == false) {
	    if (buffs.charAt(xcx) == c) {
	      cursor = xcx
	      check = true
	      return true;
	    } else {
      xcx+=1
      if (xcx == buffs.length) check = true
	    }
	  }
	  return false;
	}




	/**

	 * Search Forwards.  This generalises the character version of find forwards

	 * The user provides a predicate to specify the search condition.

	 * This operation does not change any variable other than the cursor position.

	 */

	def sf(p: Char => Boolean): Boolean = {
	  var check: Boolean = false
	  var buffs: String = ""

	  
	  var xcx = cursor
	  while (xcx < buffer.length) {
	  buffs = buffs + buffer.charAt(xcx)
	  xcx+=1
	  }
	  xcx = cursor

	  
	  while (xcx < buffs.length|| check == false) {
	    if (buffs.charAt(xcx) == p) {
	      cursor = xcx
	      check = true
	      return true;
	    } else {
      xcx+=1
      if (xcx == buffs.length) check = true
	    }
	  }
	  return false;
	}

}
