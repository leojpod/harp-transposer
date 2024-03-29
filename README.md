This is a small tool to help you quickly transform a lick from one position to another.

While there might be a dedicated UI to change to another octave directly, for now you can simply transform from one position to itself and that should do the trick to change octave.

---

## Road-map

### Version 1: no backend thing

- [x] model the harmonica holes/bends/overblows
- [x] model the chromatic scale
- [x] model the positions (link hole to a musical interval)
  - [x] 1st,
  - [x] 2nd,
  - [x] 3rd
  - more later
- [x] make a parser to identify the notation in a text box
  - [x] Parser
  - [x] UI
- [x] translate from one position to another
  - [x] function to create candidates
  - [x] UI to select the alternatives
    - [x] option to apply selection globally
  - [x] Colour feedback to show if a global change/local change has been made.
- [ ] octave transformer
  - translate from one position to the same
  - [ ] selector to choose the direction

### Version 2

- [ ] on the input page put 2 selectors: one for Key, one for position (optional).
  - [ ] on the output page show a selector that is the combination of both key and position.
- [ ] Change the colours everywhere...
- [ ] Input UI with colour support for recognised harmonica notes
- [ ] Easier edition of the translation

### How to model the harmonica layout

What we have:

- 10 holes, draw/blow
- some have 1 or more bends
- some have overblows/overdraws
