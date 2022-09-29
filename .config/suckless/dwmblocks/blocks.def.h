// Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/ /*Command*/ /*Update Interval*/ /*Update Signal*/
	{"RAM:", "~/.config/suckless/dwmblocks/scripts/memory.sh", 6, 1},

	{"", "date '+%b %d (%a) %I:%M%p'", 5, 0},
};

// sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = " | ";
static unsigned int delimLen = 5;