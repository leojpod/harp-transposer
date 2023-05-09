# Harp Transposer

## To-do

### Version 1: no backend thing

- [x] model the harmonica holes/bends/overblows
- [x] model the chromatic scale
- [ ] model the positions (link hole to a musical interval)
  - [x] 1st,
  - [x] 2nd,
  - [ ] 3rd only
- [x] make a parser to identify the notation in a text box
  - [x] Parser
  - [x] UI
- [ ] translate from one position to another
  - [x] function to create candidates
  - [x] UI to select the alternatives
    - [ ] option to apply selection globally
- [ ] octave transformer
  - translate from one position to the same
  - [ ] selector to choose the direction

### Version 2

- [ ] Input UI with colour support for recognised harmonica notes

### How to model the harmonica layout

What we have:

- 10 holes, draw/blow
- some have 1 or more bends, some have blows
- some have overblows/overdraws
