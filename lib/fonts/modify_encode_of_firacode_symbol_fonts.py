#!/usr/bin/env python3
# This file is used to generate Fira Code Memacs Symbol font farmily.
# It should be run in fontforge menu item: File > Excute Python
import fontforge

FONT_EM = 1024
FIRACODE_SYMBOL_START= 0xE100
FIRACODE_SYMBOL_END = 0xE170
OFFSET = 0x1C00

dest = fontforge.font()
dest.em = FONT_EM
#    dest.encoding = "UnicodeFull"
dest.encoding = "ISO10646"
font = fontforge.open("/Users/mephis/Library/fonts/FiraCode-Regular-Symbol.otf")
font.em = FONT_EM
inserted = []

codepoint = FIRACODE_SYMBOL_START
while codepoint < FIRACODE_SYMBOL_END:
    font.selection.select(codepoint)
    font.copy()
    dest.selection.select(codepoint + OFFSET)
    dest.paste()
    inserted.append(codepoint + OFFSET)
    codepoint += 1
print("#powerline-extras:" + str(len(inserted)))

ascent = dest.ascent
descent = dest.descent

dest.os2_winascent_add = 0
dest.os2_windescent_add = 0
dest.os2_typoascent_add = 0
dest.os2_typodescent_add = 0
dest.hhea_ascent_add = 0
dest.hhea_descent_add = 0

# print ("ASCENT: " + str(ascent))
# print ("DESCENT: " + str(descent))

dest.os2_winascent = ascent
dest.os2_windescent = descent
dest.os2_typoascent = ascent
dest.os2_typodescent = -descent
dest.hhea_ascent = ascent
dest.hhea_descent = -descent

dest.em = FONT_EM
dest.fontname = "Fira Code Regular Memacs Symbol"
dest.familyname = "Fira Code Memacs Symbol"
dest.fullname = "Fira Code Regular Memacs Symbol"
dest.appendSFNTName('English (US)', 'Preferred Family', dest.familyname)
dest.appendSFNTName('English (US)', 'Compatible Full', dest.fullname)
dest.fontname= "FiraCode-Regular-Memacs-Symbol"
dest.generate("./Desktop/FiraCode-Regular-Memacs-Symbol.otf")
