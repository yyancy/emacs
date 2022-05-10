## frequent command

* C-x C-f search for files
* C-x C-s save buffer
* C-x C-w write to a new file
* C-x s save all file


* C-x b switch buffer
* C-x C-b list all buffer

* C-/, C-_, C-x u undo

## Windows management

* C-x 0 Delete the active window
* C-x 1 delete other window
* C-x 2 split window below
* C-x 3 split window right


C-x 4 C-f Finds a file in the other window
C-x 4 d Opens M-x dired in the other window
C-x 4 C-o Displays a buffer in the other window
C-x 4 b Switches the buffer in the other window
and makes it the active window
C-x 4 0 Kills the buffer and window

C-x 5 2 Create a new frame
C-x 5 b Switch buffer in other frame
C-x 5 0 Delete active frame
C-x 5 1 Delete other frames
C-x 5 C-f Finds a file in the other window
C-x 5 d Opens M-x dired in the other window
C-x 5 C-o Displays a buffer in the other window

C-x r m Set a bookmark
C-x r l List bookmarks
C-x r b Jump to bookmark

C-x r n Store number in register
C-x r s Store region in register
C-x r SPC Store point in register
C-x r + Increment number in register
C-x r j Jump to register
C-x r i Insert content of register
C-x r w Store window configuration in register
C-x r f Store frameset in register

M-h Marks the next paragraph
C-x h Marks the whole buffer
C-M-h Marks the next defun
C-x C-p Marks the next page
M-@ Marks the next word
C-M-<SPC> and
C-M-@ Marks the next s-expression
C-<SPC>, C-g Deactivates the region

M-t transpose words
C-x C-t switch two lines

M-q Refills the paragraph point is in
C-x f Sets the fill column width
C-x . Sets the fill prefix
M-x auto-fill-mode Toggles auto-filling


C-x C-; Comment or Uncomment line
M-x comment-box Comments the region but as a box

C-x C-u Uppercases the region
C-x C-l Lowercases the region
M-x upcase-initials-region Capitalizes the region

M-c Capitalizes the next word
M-c Capitalizes the next word
M-u Uppercases the next word
M-l Lowercases the next word

C-o Inserts a blank line after point
C-x C-o      Deletes all blank lines after point
C-M-o Splits a line after point, keeping
the indentation
M-ˆ Joins the line the point is on with
the one above

M-SPC Deletes all but  space or tab
to the left and right of the point
M-x cycle-spacing As above but cycles through
all but one, all, and undo
M-\\ Deletes all spaces and tabs around

M-/ Expands word at the point using
M-x dabbrev-expand
C-M-/ Expands as much as possible, and
shows a list of possible completions

TAB Indents line using major mode’s
M-i Inserts spaces or tabs to next tab stop
M-x edit-tab-stops Edits tab stops

C-x C-f Finds a file
C-x C-r Finds a file in read only mode
C-x C-q Toggles read only mode

M-x auto-revert-mode Reverts buffer when file changes
M-x auto-revert-tail-mode Appends changes when file changes