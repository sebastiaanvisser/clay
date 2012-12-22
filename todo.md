# Meta:

- First todo: move todos into bug tracker or something.
- Test project site in other browsers + mobile.

# Pretty printing:

- Do not pretty print redundant * before refinements.
- Fix mostly harmless but annoying exponential blowup when using <>
  multiple times in selectors.
- Print RGB values to hex syntax, only use RGBA for non opaque colors.
- Try to strip even more whitespace from compacted version.
- Try to avoid changing order too much when printing either nested rules or
  media queries.

# Style rules:

- Add not selector.
- Maybe ditch the Num instances for sizes.
- Add more size types.
- Check if we need more color types.
- Better support for computing with colors.
- Allow multiple box-shadows.
- Implement better model for gradient. (meh)
- Fix the Direction/Position/Sided/Location mess.
- Add all of CSS, http://www.w3.org/TR/CSS21/propidx.html.

# Future:

- Render for one browser only?
- Can we optimize duplicate style rules into one?
- Can we validate the styles against an HTML file?
- Give hints for fusing rules using classes?
- Can we run Clay in the client using Fay?
- Add dynamic values like scrollTop and pageWidth?

